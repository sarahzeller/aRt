library(tidyverse)
library(osmdata)
library(sf)
library(showtext)

font_add_google("Special Elite", "elite")
showtext_auto()

# decide on letter per elevation rank
chars_map <- data.frame(value_letter = c("", "\u2022", "I", "H", "M"),
                        value = -1:3)

# load data
# national park
extract_polygon <- function(park_name) {
  opq(bbox = "Europe") |>
    add_osm_feature(key = "name", value = park_name) |>
    osmdata_sf() |>
    # just get the lines
    pluck("osm_lines") |>
    st_union() |>
    st_polygonize() |>
    st_collection_extract("POLYGON") |>
    st_as_sf()
}

saxonian <- extract_polygon("Nationalpark Sächsische Schweiz")
bohemian <- extract_polygon("Národní park České Švýcarsko")

national_park <- saxonian |>
  st_union(bohemian)

# river
elbe <- opq(bbox = "Europe") |>
  add_osm_feature(key = "name", value = "Elbe") |>
  osmdata_sf() |>
  # just get the lines
  pluck("osm_multilines") |>
  # select main river
  filter(osm_id == 123822 & role == "main_stream") |> 
  st_crop(st_buffer(national_park, 10))

# create raster and convert back to sf
# add elevation to national park
elev_data <- elevatr::get_elev_raster(locations = national_park,
                                      z = 8,
                                      clip = "locations")
# tidy up
elbe_raster <- elbe |> 
  sf::st_buffer(100) |> 
  stars::st_rasterize() |>
  terra::rast() |> 
  terra::project(elev_data |> terra::rast())
elbe_raster[elbe_raster == 1] <- 0

elbe_buffer <- elbe |> 
  sf::st_buffer(400) |> 
  stars::st_rasterize() |>
  terra::rast() |> 
  terra::project(elev_data |> terra::rast())

park_with_river <- terra::rast(elev_data) |> 
  # merge with elbe
  terra::mosaic(elbe_raster, fun = "min") |> 
  terra::mosaic(elbe_buffer, fun = "min") |> 
  as.data.frame(xy = TRUE) |> 
  st_as_sf(coords = c("x", "y"), crs = st_crs(elbe)) |>
  rename_with(\(x) ifelse(x != "geometry", "value", x)) |> 
  # classify: elbe 0, otherwise rank
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
      lineheight = .5,
      colour = "grey10",
      hjust = .8
    )
  )

ggsave("national_park.png", bg = "white")
