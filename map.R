library(tidyverse)
library(osmdata)
library(sf)
library(showtext)

font_add_google("Special Elite", "elite")
showtext_auto()

extract_polygon <- function(park_name) {
  opq(bbox = "Europe") |> 
    add_osm_feature(key = "name",
                    value = park_name) |> 
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

# add elevation
elev_data <- elevatr::get_elev_raster(locations = national_park,
                                      z = 8,
                                      clip = "locations")

# tidy elevation
elev_df <- elev_data |> 
  terra::rast() |> 
  as.data.frame(xy = TRUE) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326)
names(elev_df)[1] <- "value"

elev_df |> mutate(class = ntile(value, 4)) |> View()

# plot

ggplot() +
  geom_sf(data = elev_df, aes(col = value)) +
  scale_colour_continuous(na.value = "white") +
  theme_void()
