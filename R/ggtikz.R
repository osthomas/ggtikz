#' Create a canvas and add a TikZ annotation.
#'
#' This is a helper function for quick one-step annotations. It creates a
#' ggtikzCanvas from a ggplot, adds one annotation to it, and optionally
#' draws the plot and the annotations.
#'
#' For finer control, see `ggtikzCanvas()` and `ggtikzAnnotation()`.
#'
#' @param gg_plot A ggplot object on which annotations should be made.
#' @param ... Passed to \code{\link{ggtikzAnnotation}}.
#' @param draw TRUE or FALSE. Should gg_plot and the resulting annotation be
#'   drawn immediately? A tikz device needs to be open.
#'
#' @seealso \code{\link{ggtikzCanvas}} for creating a canvas which can store
#'   multiple annotations.
#' @seealso \code{\link{ggtikzAnnotation}} for creating an annotation, which can
#'   then be added to a canvas.
#'
#' @returns A \code{\link{ggtikzCanvas}} object with one
#'   \code{\link{ggtikzAnnotation}} (specified in `...`) already added. If
#'   `draw = TRUE`, the `gg_plot` and the annotations are drawn to the currently
#'   active device. This must be a `tikzDevice`, or an error will be raised.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(tikzDevice)
#' library(ggtikz)
#' p <- ggplot(mtcars, aes(disp, mpg)) + geom_point()
#' out <- tempfile(fileext = ".tikz")
#' tikz(out)
#' # Add a red circle in the middle of the plot.
#' ggtikz(p, "\\fill[red] (0.5,0.5) circle (2mm);", xy="plot")
#' dev.off()
#' }
#' @export
ggtikz <- function(gg_plot, ..., draw = TRUE) {
    canvas <- ggtikzCanvas(gg_plot)
    annotation <- ggtikzAnnotation(...)

    canvas <- canvas + annotation

    if (draw) {
        print(gg_plot)
        print(canvas)
    }

    return(canvas)
}
