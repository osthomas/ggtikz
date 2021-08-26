#' Split a TikZ coordinate.
#'
#' @param coord Coordinate string of the form "(x,y)"
#'
#' @returns A character vector of length 2: The x and y components of the
#'  coordinate. These may contain spaces.
split_coord <- function(coord) {
    pattern_components <- "\\(([^,]*),([^\\)]*)\\)"
    components <- stringr::str_match(coord, pattern_components)
    x <- components[1,2]
    y <- components[1,3]

    return(c(x, y))
}


replace_coords <- function(tikz_code, replace_func) {
    pattern_coord <- "\\([^\\)]*,.*?\\)"
    result <- stringr::str_replace_all(tikz_code, pattern_coord, replace_func)

    return(result)
}
