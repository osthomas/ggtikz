#' Replace Inf in TikZ coordinates
#'
#' Infinite values in TiKZ coordinate specifications are replaced by values
#' corresponding to the edge of the available coordinate space. This allows
#' placement of annotations at the very edge of a panel without knowing its
#' precise coordinates. This is useful for annotations which extend
#' to the panel boundaries, but also make use of specific coordinates.
#'
#' @param ggtikzCanvas A \code{link{ggtikzCanvas}} object.
#' @param ggtikzAnnotation A \code{link{ggtikzAnnotaton}} object.

#' @returns A \code{link{ggtikzAnnotation}} object, with Infinites in coordinates
#'  replaced by finite values.
ggtikzUninfinite <- function(ggtikzCanvas, ggtikzAnnotation) {
    if (ggtikzAnnotation$replace_inf && !requireNamespace("stringr"))
        stop("ggtikzUninfinite requires the `stringr` package.")

    reference <- ggtikzAnnotation$reference
    ranges <- get_panel_range(
        ggtikzCanvas,
        ggtikzAnnotation$panelx,
        ggtikzAnnotation$panely)

    if (reference[["x"]] == "data") {
        xrange <- ranges$x
    } else {
        xrange <- c(0, 1)
    }
    if (reference[["y"]] == "data") {
        yrange <- ranges$y
    } else {
        yrange <- c(0, 1)
    }

    transformed <- uninfinite_tikz(
        ggtikzAnnotation$tikz_code,
        xrange,
        yrange)
    ggtikzAnnotation$tikz_code <- transformed

    return(ggtikzAnnotation)
}


#' Replace infinite values in TikZ coordinates
#'
#' Infinite values are replaced with the minimum or maximum value of the padding
#' in the x or y direction, respectively. Additionally, the adjusted coordinate
#' is padded so that it lies just next to the panel borders and axis lines
#' without overlap.
#'
#' @param coord TikZ coordinate
#' @param xrange Numeric vector of length 2, minimum and maximum values in the x
#'  direction
#' @param yrange Numeric vector of length 2, minimum and maximum values in the y
#'  direction
#' @returns The adjusted TikZ coordinate with padding, as a string.
uninfinite_coord <- function(coord, xrange, yrange) {
    coord_split <- split_coord(coord)
    # coord might contain units, therefore suppress warnings for conversion to
    # numeric
    coord_values <- suppressWarnings(as.numeric(coord_split))

    # No infinite values: do nothing
    if (!any(is.infinite(coord_values))) return(coord)
    discretized <- discretize(coord_values, xrange, yrange)

    # NA for values with units: do not alter output
    discretized[is.na(discretized)] <- coord_split[is.na(discretized)]

    new_coord <- sprintf("(%s,%s)", discretized[1], discretized[2])

    return(new_coord)
}


#' @rdname uninfinite_coord
#' @param tikz_code The TikZ code to replace Infinite values in.
uninfinite_tikz <- function(tikz_code, xrange, yrange) {
    replace_func <- function(coord) {
        uninfinite_coord(coord, xrange, yrange)
    }
    result <- replace_coords(tikz_code, replace_func)
}


#' Replace Infinites by discrete values
#'
#' The replacement values correspond to the edges of the available coordinate
#' space
#'
#' @param coord_values numeric. The coordinate x and y values, potentially
#'  containing Inf or -Inf
#' @inheritParams uninfinite_coord
discretize <- function(coord_values, xrange, yrange) {
    x <- coord_values[1]
    y <- coord_values[2]

    mins <- c(xrange[1], yrange[1])
    maxs <- c(xrange[2], yrange[2])

    inf <- is.infinite(coord_values)
    neg <- coord_values < 0

    coord_values[inf & neg] <- mins[inf & neg]
    coord_values[inf & !neg] <- maxs[inf & !neg]

    return(coord_values)
}
