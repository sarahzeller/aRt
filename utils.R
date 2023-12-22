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

extract_polygon <- function(osm_data) {
  osm_data |>
    # just get the lines
    pluck("osm_lines") |>
    st_union() |>
    st_polygonize() |>
    st_collection_extract("POLYGON") |>
    st_as_sf()
}

make_raster <- function(polygon,
                        template = elev_data) {
  polygon |>
    stars::st_rasterize() |>
    terra::rast() |>
    terra::project(template |> terra::rast())
}