library(tidyverse)
library(osmdata)
library(sf)
library(showtext)
here("utils.R") |> source()

font_add_google("Special Elite", "elite")
showtext_auto()

# decide on letter per elevation rank
chars_map <- data.frame(value_letter = c("", "\u2022", "I", "H", "M"),
                        value = -1:3)

# load data
# national park
saxonian <- extract_polygon("Nationalpark Sächsische Schweiz")
bohemian <- extract_polygon("Národní park České Švýcarsko")

national_park <- bohemian |>
  st_union(saxonian) |> 
  # resolve boundaries and return to sf class
  st_union() |> 
  st_as_sf()

# river
elbe <- opq(bbox = "Europe") |>
  add_osm_feature(key = "name", value = "Elbe") |>
  osmdata_sf() |>
  # just get the lines
  pluck("osm_multilines") |>
  # select main river
  filter(osm_id == 123822 & role == "main_stream") |> 
  st_crop(st_buffer(national_park, 200))

# create raster and convert back to sf
# get elevation for national park bounding box
elev_data <- elevatr::get_elev_raster(locations = st_buffer(national_park, 0),
                                      z = 8,
                                      clip = "bbox")
# get rasterized line data for river and national park borders
elbe_raster <- elbe |> 
  sf::st_buffer(100) |> 
  make_raster()
elbe_raster[elbe_raster == 1] <- 0

border_raster <- national_park |> 
  st_cast("MULTILINESTRING") |> 
  sf::st_buffer(5) |> 
  make_raster()
  
park_with_river <- terra::rast(elev_data) |> 
  # merge with river and border
  terra::mosaic(elbe_raster, fun = "min") |> 
  terra::mosaic(border_raster, fun = "min") |> 
  # convert to sf
  as.data.frame(xy = TRUE) |> 
  st_as_sf(coords = c("x", "y"), crs = st_crs(elbe)) |>
  rename_with(\(x) ifelse(x != "geometry", "value", x)) |> 
  # classify: elbe 0, border -1, otherwise rank
  mutate(value = case_when(value == 0 ~ 0,
                           value == 1 ~ -1, 
                           .default = ntile(value, 3))) |> 
  # add letter
  left_join(chars_map, by = "value")

# plot
ggplot() +
  geom_sf_text(
    data = park_with_river,
    aes(label = value_letter),
    size = 2,
    family = "elite"
  ) +
  labs(title = "Elbsandstein-\nGebirge") +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(
      family = "elite",
      size = 40,
      lineheight = .3,
      colour = "grey10",
      hjust = .8
    )
  )

ggsave("national_park.png", bg = "white")
