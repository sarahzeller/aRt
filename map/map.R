library(tidyverse)
library(osmdata)
library(sf)
library(showtext)
library(here)
here("map/utils.R") |> source()

font_add_google("Special Elite", "elite")
showtext_auto()

# decide on letter per elevation rank
chars_map <- data.frame(value_letter = c("", "\u2022", "I", "H", "M"),
                        value = -1:3)

# load data
# national park
saxonian <- load_and_save_osm("Nationalpark Sächsische Schweiz") |> 
  pluck("osm_multipolygons")

bohemian <- load_and_save_osm("Národní park České Švýcarsko") |> 
  pluck("osm_multipolygons")

national_park <- bohemian |>
  st_union(saxonian) |> 
  # resolve boundaries and return to sf class
  st_union() |> 
  # cast to simlified multilinestring
  st_buffer(1) |>
  st_cast("MULTILINESTRING") 

# river
elbe <- load_and_save_osm("Elbe") |> 
  # just get the lines
  pluck("osm_multilines") |>
  # select main river
  filter(osm_id == 123822 & role == "main_stream") |> 
  st_crop(st_buffer(national_park, 2000))

# create raster and convert back to sf
# get elevation for national park bounding box
elev_data <- elevatr::get_elev_raster(locations = st_as_sf(national_park),
                                      z = 8,
                                      clip = "bbox") |> 
  terra::rast()

# get rasterized line data for river and national park borders
elbe_raster <- elbe |> 
  sf::st_simplify(dTolerance = 400) |>
  terra::rasterize(elev_data) |> 
  # set value to 0
  tidyterra::mutate(layer = ifelse(layer == 1, 0, layer))

border_raster <- national_park |> 
  sf::st_simplify(dTolerance = 400) |>
  terra::vect() |> 
  terra::rasterize(elev_data)
  
park_with_river_raster <- elev_data |> 
  # merge with river and border
  terra::mosaic(elbe_raster, fun = "min") |> 
  terra::mosaic(border_raster, fun = "min") |> 
  # classify values
  tidyterra::rename_with(\(x) ifelse(x != "geometry", "value", x)) |> 
  tidyterra::mutate(value = case_when(value == 0 ~ 0,
                           value == 1 ~ -1, 
                           .default = ntile(value, 3))) 

park_with_river <- park_with_river_raster |> 
  # convert to sf
  as.data.frame(xy = TRUE) |>
  st_as_sf(coords = c("x", "y"), crs = st_crs(elbe)) |>
  # add letter
  left_join(chars_map, by = "value")

# plot
ggplot() +
  geom_sf_text(
    data = park_with_river,
    aes(label = value_letter),
    size = 8,
    family = "elite"
  ) +
  labs(title = "Elbsandsteingebirge") +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(
      family = "elite",
      size = 100,
      margin = margin(t = 30),
      hjust = .08
    )
  )

ggsave("map/national_park.png", bg = "white",
       width = 297,
       height = 210,
       units = "mm")
