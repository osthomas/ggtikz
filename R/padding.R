
get_padding_panel <- function(gg_plot) {
    # Pad with the panel border size, or the axis line size, whichever is larger
    padding <- get_padding_from_elements(
        gg_plot,
        c("panel.border", "axis.line.x.top"),
        c("panel.border", "axis.line.y.right"),
        c("panel.border", "axis.line.x.bottom"),
        c("panel.border", "axis.line.y.left")
    )

    return(padding)
}


get_padding_plot <- function(gg_plot) {
    # Pad with the plot background size
    padding <- get_padding_from_elements(
        gg_plot,
        "plot.background",
        "plot.background",
        "plot.background",
        "plot.background"
    )

    return(padding)
}


#' Calculate length of padding from plot elements
#'
#' To prevent overlap with panel borders or axis lines, annotations are clipped
#' to a viewport that is reduced in size by the width of these lines.
#' They depend on the current plot theme.
#'
#' @param gg_plot A ggplot2 object.
#' @param elements_t character vector with names of elements to consider for padding at the *top*
#' @param elements_r character vector with names of elements to consider for padding on the *right*
#' @param elements_b character vector with names of elements to consider for padding at the *bottom*
#' @param elements_l character vector with names of elements to consider for padding on the *left*
#' @returns A vector `grid::unit`s of paddings for `t`, `r`, `b`, `l` (in pt)
#'
#' @seealso \code{\link{uninfinite_coord}} for construction of the complete
#'  replaced coordinate.
get_padding_from_elements <- function(gg_plot, elements_t, elements_r, elements_b, elements_l) {
    # Get the theme used for the plot
    p_theme <- gg_plot$theme
    class(p_theme) <- class(ggplot2::theme)
    p_theme <- ggplot2::theme_get() + p_theme

    elements <- unique(c(elements_t, elements_r, elements_b, elements_l))

    lwds <- sapply(elements, function(element) {
        el <- ggplot2::calc_element(element, p_theme)
        grob <- ggplot2::element_grob(el)
        lwd <- grob$gp$lwd
        if (is.null(lwd) || is.na(lwd)) lwd <- 0
        return(lwd)
    }, USE.NAMES = TRUE)

    padding <- c(
        t = max(lwds[elements_t]),
        r = max(lwds[elements_r]),
        b = max(lwds[elements_b]),
        l = max(lwds[elements_l])
    )
    # Pad with half the line width, because half of the line extends out of the
    # plot area. Apply the tikzDevice tikzLwdUnit factor.
    padding <- grid::unit(padding, "pt") * getOption("tikzLwdUnit") * 0.5
    return(padding)
}
