load_and_save_osm <- function(value_name) {
  if (!paste0(value_name, ".rds") %in% list.files(here())) {
    # load
    osm_data <- opq(bbox = "Europe") |>
      add_osm_feature(key = "name", value = value_name) |>
      osmdata_sf()
    # save
    saveRDS(osm_data, paste0(value_name, ".rds") |> here())
    # return
    return(osm_data)
  } else {
    paste0(value_name, ".rds") |>
      here() |>
      readRDS()
  }
}

make_raster <- function(polygon,
                        template = elev_data) {
  polygon |>
    stars::st_rasterize() |>
    terra::rast() |>
    terra::project(template |> terra::rast())
}