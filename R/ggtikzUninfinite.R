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

    padding <- get_padding(ggtikzCanvas$p)
    transformed <- uninfinite_tikz(
        ggtikzAnnotation$tikz_code,
        xrange,
        yrange,
        padding)
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
#' @param padding Character vector of length 2. Coordinate components with which
#'  the resulting adjusted coordinate will be padded, using the TikZ 'calc' library
#'  notation: ($(adjusted_x, adjusted_y)++(pad_x, pad_y)$)
#' @returns The adjusted TikZ coordinate with padding, as a string.
uninfinite_coord <- function(coord, xrange, yrange, padding) {
    coord_split <- split_coord(coord)
    # coord might contain units, therefore suppress warnings for conversion to
    # numeric
    coord_values <- suppressWarnings(as.numeric(coord_split))

    # No infinite values: do nothing
    if (!any(is.infinite(coord_values))) return(coord)
    discretized <- discretize(coord_values, xrange, yrange)

    # NA for values with units: do not alter output
    discretized[is.na(discretized)] <- coord_split[is.na(discretized)]
    adjusted_pad <- adjust_padding(coord_values, padding)

    new_coord <- do.call(
        sprintf, c(
            fmt = "($(%s,%s)+(%s,%s)$)",
            as.list(c(discretized, adjusted_pad)))
    )

    # coord_new <- sprintf("(%s,%s)", x_out, y_out)

    return(new_coord)
}


#' @rdname uninfinite_coord
#' @param tikz_code The TikZ code to replace Infinite values in.
uninfinite_tikz <- function(tikz_code, xrange, yrange, padding) {
    replace_func <- function(coord) {
        uninfinite_coord(coord, xrange, yrange, padding)
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


#' Calculate length of padding from plot borders
#'
#' To prevent overlap with panel borders or axis lines, Infinites in TikZ
#' coordinates are padded with the thickness of these lines. They depend on the
#' current plot theme.
#'
#' @param gg_plot A ggplot2 object.
#' @returns A character vector of paddings for `t`, `r`, `l`, `b`.
#'
#' @seealso \code{\link{uninfinite_coord}} for construction of the complete
#'  replaced coordinate.
get_padding <- function(gg_plot) {
    # Get the theme used for the plot
    p_theme <- gg_plot$theme
    class(p_theme) <- class(ggplot2::theme)
    p_theme <- ggplot2::theme_get() + p_theme

    # Get the lwd of the border and axis lines
    elements <- c(
        "panel.border",
        "axis.line.x.top",
        "axis.line.x.bottom",
        "axis.line.y.left",
        "axis.line.y.right")
    lwds <- lapply(elements, function(element) {
        el <- ggplot2::calc_element(element, p_theme)
        grob <- ggplot2::element_grob(el)
        lwd <- grob$gp$lwd
        if (is.null(lwd) || is.na(lwd)) lwd <- 0
        return(lwd)
    })
    names(lwds) <- elements

    # Pad with the panel border size, or the axis line size, whichever is larger
    padding <- c(
        t = max(lwds$panel.border, lwds$axis.line.x.top),
        r = max(lwds$panel.border, lwds$axis.line.y.right),
        b = max(lwds$panel.border, lwds$axis.line.x.bottom),
        l = max(lwds$panel.border, lwds$axis.line.y.left)
    )

    # tikzDevice converts from R lwd to TikZ pt with the tikzLwdUnit factor
    padding <- padding * getOption("tikzLwdUnit")

    # tikzDevice rounds to one digit
    padding <- round(padding, 1)

    # We need to pad half the line width
    padding <- padding/2
    padding <- paste0(padding, "pt")
    names(padding) <- c("t", "r", "b", "l")

    return(padding)
}


#' Adjust padding for Infinite coordinates
#'
#' Add appropriate sign to the padding values, depending on the sign of the
#' infinity.
#'
#'  * -Inf has to be shifted in the positive direction
#'  * +Inf has to be shifted in the negative direction
#'
#' @inheritParams discretize
#' @param padding Result of \code{\link{get_padding}}
#' @returns A character vector of adjusted padding lengths.
adjust_padding <- function(coord_values, padding) {
    inf <- is.infinite(coord_values)
    neg <- coord_values < 0

    mins <- padding[c("l", "b")]
    maxs <- padding[c("r", "t")]

    padding_out <- c("0", "0")
    padding_out[inf & neg] <- mins[inf & neg]
    padding_out[inf & !neg] <- paste0("-", maxs[inf & !neg])
    return(padding_out)
}
