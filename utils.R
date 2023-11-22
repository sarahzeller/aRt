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

make_raster <- function(polygon, 
                        template = elev_data){
  polygon |> 
    stars::st_rasterize() |>
    terra::rast() |> 
    terra::project(template |> terra::rast())  
}