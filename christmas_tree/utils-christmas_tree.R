#' Mirror points
#' 
#' @param points A data.frame containing the points that should be mirrored. Should contain 'x' and 'y' info.
#' @return A longer data.frame with the mirrored points and the first point as last row.
#' 
#' @example 
#' test <- tibble::tribble(
#'  ~x, ~y,
#'  -.5, 1,
#'  -.5, 2)
#' mirror(test)

mirror <- function(points){
  reverse_ordering <- points |> 
    row.names() |> 
    desc()
  
  other_side <- points |> 
    arrange(reverse_ordering) |> 
    mutate(x = x * -1)
  
  points |> 
    # add other side
    rbind(other_side) |> 
    # return to origin
    close_polygon()
}

#' Close a polygon by adding the first line as the last line
#' 
#' @param points the data.frame with the x and y as points.

close_polygon <- function(points) {
  points |> 
    rbind(points[1,])
}


#' Make a polygon from points
#' 
#' @param points A dataframe with x and y containing the points that should be turned into a polygon

make_polygon <- function(points){
  points |> 
    as.matrix() |> 
    list() |> 
    st_polygon()
}


#' Create a tab within paste
#' 
#' @param tab_width How many spaces should be created as a tab
#' 
tab <- function(tab_width){
  paste0(
    "<span style='color:transparent;'>",
    rep(".", tab_width) |> paste(collapse = ""),
    "</span>"
  )
}
