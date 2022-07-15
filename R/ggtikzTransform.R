#' Transform TikZ coordinates according to scale transformations
#'
#' ggtikzTransform extracts coordinates definitions in an annotation's TikZ code
#' and transforms them with the transformer functions stored in the underlying
#' plot's x or y scales, respectively.
#'
#' This function does not have to called directly. It is automatically called
#' when annotations are added to a canvas, if `transform = TRUE` in the
#' `ggtikzAnnotation` construction call.
#'
#' Coordinates components with physical lengths are not changed.
#' For a plot with a linear x scale and a log10-transformed y scale,
#'
#' * the TikZ coordinate (10,10) becomes (10,1),
#' * the TikZ coordinate (10cm,10) becomes (10cm,1),
#' * the TikZ coordinate (10,10cm) becomes (10,10cm)
#' * the TikZ coordinate (0,0) will raise an error.
#'
#' @param ggtikzCanvas A \code{link{ggtikzCanvas}} object.
#' @param ggtikzAnnotation A \code{link{ggtikzAnnotaton}} object.
#'
#' @returns A \code{link{ggtikzAnnotation}} object, with transformations applied
#'   to the coordinates in the TikZ code.
ggtikzTransform <- function(ggtikzCanvas, ggtikzAnnotation) {
    if (ggtikzAnnotation$.transformed) {
        # Already transformed, nothing to do.
        return(ggtikzAnnotation)
    } else {
        panelx <- ggtikzAnnotation$panelx
        panely <- ggtikzAnnotation$panely
        transforms <- get_panel_transforms(ggtikzCanvas, panelx, panely)

        # Do not transform panel and plot references
        reference <- ggtikzAnnotation$reference
        for (name in names(reference)) {
            if (reference[[name]] != "data") transforms[[name]] <- identity
        }
        transformed <- transform_tikz(
            ggtikzAnnotation$tikz_code,
            transforms$x,
            transforms$y)
        ggtikzAnnotation$tikz_code <- transformed
        ggtikzAnnotation$.transformed <- TRUE

        return(ggtikzAnnotation)
    }
}



try_transform <- function(x, transform_fun) {
    # Try to convert the value to a numeric vector
    # If this returns NA, there is a unit behind the value, and we return
    # unchanged (e.g. for '3cm', '1.5 in').
    new <- suppressWarnings(as.numeric(x))
    if (is.na(new)) return(x)

    # If the coordinate is an explicit Inf/-Inf, do not transform it:
    # It will later be adjusted to correspond to the minimum/maximum of the
    # respective reference frame
    if (is.infinite(new)) return(x)

    new <- suppressWarnings(transform_fun(new))
    if (is.infinite(new) | is.na(new)) {
        # Something went wrong
        func_str <- paste0("  ", deparse(transform_fun), collapse="\n  ")
        msg <- sprintf(
            "The value %s could not be transformed with function:\n%s",
            x, func_str)
        stop(msg)
    }

    return(new)
}


transform_coord <- function(coord, transform_x, transform_y) {
    coord <- split_coord(coord)
    x_orig <- coord[1]
    y_orig <- coord[2]
    x_new <- try_transform(x_orig, transform_x)
    y_new <- try_transform(y_orig, transform_y)
    coord_new <- sprintf("(%s,%s)", x_new, y_new)

    return(coord_new)
}


transform_tikz <- function(tikz_code, transform_x, transform_y) {
    replace_func <- function(coord) {
        transform_coord(coord, transform_x, transform_y)
    }
    result <- replace_coords(tikz_code, replace_func)

    return(result)
}
