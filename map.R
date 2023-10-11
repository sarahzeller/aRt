library(tidyverse)
library(osmdata)
library(sf)
library(showtext)

font_add_google("Special Elite", "elite")
showtext_auto()

# decide on letter per elevation rank
chars_map <- data.frame(value_letter = c("I", "H", "M"),
                        value = 1:3)

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
elev_df <- elev_data |>
  terra::rast() |>
  as.data.frame(xy = TRUE) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  rename_with(\(x) ifelse(x != "geometry", "value", x)) |>
  # classify
  mutate(value = ntile(value, 3)) |>
  # add letter
  left_join(chars_map, by = "value")

elbe_raster <- stars::st_rasterize(elbe) |> 
  as.data.frame(xy = TRUE) |> 
  drop_na(ID) |>
  mutate(value_letter = "-") |> 
  select(-ID) |>
  st_as_sf(coords = c("x", "y"), crs = st_crs(elbe))

# plot
ggplot() +
  geom_sf_text(
    data = elev_df,
    aes(label = value_letter),
    size = 2,
    family = "elite"
  ) +
  geom_sf_text(
    data = elbe_raster,
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
      size = 20,
      colour = "grey10",
      hjust = .8
    )
  )

ggsave("national_park.png")