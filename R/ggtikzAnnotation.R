#' Prepare a TikZ annotation for a ggplot.
#'
#' ggtikzAnnotation objects are meant to be added to a ggtikzCanvas object.
#'
#' This function prepares TikZ annotations in a form understandable to a
#' ggtikzCanvas object.
#' An annotation can be added to multiple ggtikzCanvas objects, provided that each
#' underlying ggplot object has the necessary panels to know what to do with this
#' information.
#'
#' @param tikz_code The tikz code to use for annotation. Backslashes must be escaped!
#' @param x Reference frame for the x coordinates. Either "data" or "panel".
#' @param y Reference frame for the y coordinates. Either "data" or "panel".
#' @param xy Reference frame for both x and y coordinates. Trumps `x` and `y`. Either "data" or "panel" or "plot".
#' @param panelx x position of the panel to use as coordinate reference, starting from the left, 1-based.
#' @param panely y position of the panel to use as coordinate reference, starting from the top, 1-based.
#' @param clip Should annotations be clipped to the panel boundaries?
#'    See the `clip` argument to \code{\link[grid]{viewport}}
#'
#' @returns A ggtikzAnnotation object, which can be added to a ggtikzCanvas object.
#'
#' @seealso \code{\link[tikzDevice]{grid.tikzAnnotate}} for annotation of base graphics
#' @seealso \code{\link{ggtikz}} for a helper function for quick one-step annotations.
#' @seealso \code{\link{ggtikzCanvas}} for information about initiating the annotation process.
#'
#' @export
ggtikzAnnotation <- function(
    tikz_code,
    x = c("data", "panel"),
    y = c("data", "panel"),
    xy = NULL,
    panelx = NULL, panely = NULL,
    clip = "inherit"
) {
    if (!is.null(xy)) {
        xy <- match.arg(xy, choices = c("data", "panel", "plot"))
        x <- xy
        y <- xy
    } else {
        x <- match.arg(x)
        y <- match.arg(y)
    }
    reference <- c(x, y)
    names(reference) <- c("x", "y")

    if (any(reference != "plot") & (is.null(panelx) | (is.null(panely)))) {
        stop("For data and panel reference frames, `panelx` and `panely` must be given.")
    }
    if (any(reference == "plot")) {
        if (!is.null(panelx) | !(is.null(panely))) {
            warning("`panelx` and `panely` are ignored for plot reference frames.")
        }
        panelx <- 1
        panely <- 1
    }

    # Set up a multiplier, depending on the reference frame.
    # It must be smaller for large viewports, such as the whole plot,
    # to avoid 'Dimension too large' error from LaTeX when rendering.
    if (any(reference == "plot")) {
        .mult <- 5
    } else {
        .mult <- 50
    }

    self <- structure(
        list(
            tikz_code = tikz_code,
            reference=reference,
            panelx = panelx,
            panely = panely,
            clip = clip,
            .mult = .mult,
            .id = NULL),
        class = "ggtikzAnnotation"
    )

    return(self)
}


#' @export
get_annotation_name.ggtikzAnnotation <- function(self) {
    id <- self$.id
    if (is.null(id)) {
        stop("An annotation name can only be given once the annotation has been added to a ggtikz object.")
    }

    return (paste("ggtikzannotation", id, sep="_"))
}
