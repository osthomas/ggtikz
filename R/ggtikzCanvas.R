#' Create a canvas to store TikZ annotations to a ggplot.
#'
#' Annotations can be made relative to the whole plot, to a panel, or to data
#' coordinates (of individual panels).
#'
#' This function provides a canvas for TikZ annotations, and does not draw
#' anything by itself. Its purpose is to provide information about the
#' underlying ggplot object for coordinate calculations.
#'
#' @param gg_plot A ggplot object on which annotations should be made.
#'
#' @returns A ggtikzCanvas object, to which annotations can be added.
#'
#' @seealso \code{\link[tikzDevice]{grid.tikzAnnotate}} for annotation of base
#'   graphics.
#' @seealso \code{\link{ggtikz}} for a helper function for quick one-step
#'   annotations.
#' @seealso \code{\link{ggtikzAnnotation}} for more information about creating
#'   and adding ggtikz annotations.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(tikzDevice)
#' library(ggtikz)
#' p <- ggplot(mtcars, aes(disp, mpg)) + geom_point()
#'
#' # Create a TikZ canvas on the plot
#' canvas <- ggtikzCanvas(p)
#'
#' # Create annotations to add to the canvas
#'
#' # Circle in the center of the plot
#' annotation1 <- ggtikzAnnotation(
#'    "\\fill[red] (0.5,0.5) circle (2mm);",
#'    xy = "plot")
#'
#' # Arrow to data coordinate (400,20)
#' annotation2 <- ggtikzAnnotation(
#'    "\\draw[<-] (400,20) -- ++(0,2.5);",
#'    xy = "data", panelx = 1, panely = 1)
#'
#' out <- tempfile(fileext = ".tikz")
#' tikz(out)
#' # First, draw the original plot
#' p
#' # Then, add the annotations to the canvas and draw it
#' canvas + annotation1 + annotation2
#' dev.off()
#' }
#'
#' @export
ggtikzCanvas <- function(gg_plot) {
    # For coordinate calculations and panel localisation, information at different
    # steps of the ggplot build process is required.
    # Calculate them once for later access.
    built <- ggplot2::ggplot_build(gg_plot)
    gtable <- ggplot2::ggplot_gtable(built)

    self <- structure(
        list(
            p = gg_plot,
            built = built,
            gtable = gtable,
            panel_info = NULL,
            .annotations = list()),
        class = "ggtikzCanvas"
    )
    panel_info <- get_panel_info(self)
    self$panel_info <- panel_info
    invisible(self)
}


#' @export
print.ggtikzCanvas <- function(x, ...) draw_annotations(x)


#' @export
get_panel_info.ggtikzCanvas <- function(self) {
    gtable <- self$gtable
    built <- self$built

    panel_info <- gtable$layout[grepl("^panel", gtable$layout$name),]

    # The position of the panels in the panel matrix (corresponding to visual)
    panel_info$COL <- dplyr::dense_rank(panel_info$l)
    panel_info$ROW <- dplyr::dense_rank(panel_info$t)

    # Match the indices of the panels in the built$layout$panel_params list to
    # the ROWs and COLs of the panels
    panel_layout <- built$layout$layout[, c("PANEL", "ROW", "COL")]
    panel_info <- merge(panel_layout, panel_info, by = c("ROW", "COL"))
    panel_info$PANEL <- as.numeric(as.character(panel_info$PANEL))

    panel_info <- panel_info[, c("ROW", "COL", "PANEL", "t", "l", "b", "r", "name")]
    panel_info$vp_name <- with(
        panel_info,
        sprintf("%s.%d-%d-%d-%d", name, t, r, b, l))

    return(panel_info)
}


#' @export
get_panel_range.ggtikzCanvas <- function(self, panelx, panely) {
    idx <- get_panel_index(self, panelx, panely)
    panel_n <- self$panel_info[idx, "PANEL"]
    xrange <- self$built$layout$panel_params[[panel_n]]$x.range
    yrange <- self$built$layout$panel_params[[panel_n]]$y.range
    ranges <- list(x = xrange, y = yrange)

    return(ranges)
}


#' @export
get_panel_transforms.ggtikzCanvas <- function(self, panelx, panely) {
    idx <- get_panel_index(self, panelx, panely)
    panel_n <- self$panel_info[idx, "PANEL"]
    trans_x <- self$built$layout$panel_params[[panel_n]]$x$scale$trans$transform
    trans_y <- self$built$layout$panel_params[[panel_n]]$y$scale$trans$transform

    transforms <- list(x = trans_x, y = trans_y)

    return(transforms)
}


#' Convert data coordinates to npc coordinates.
#'
#' @param self a \code{\link{ggtikzCanvas}} object
#' @param coord A numeric vector of length 2, with the x coordinate to convert
#'   at `coord[1]` and the y coordinate to convert at `coord[2]`
#' @param panelx X position (column) of the panel holding the data
#' @param panely X position (row) of the panel holding the data
#'
#' @returns The input coordinates from `coord` converted to npc coordinates in
#'   the form of a numeric vector of length 2. (0,0) corresponds to the lower
#'   left corner of the viewport containing the `ggplot` panel specified by
#'   `panelx` and `panely`, and (1,1) corresponds to the upper right corner.
#'
#' @export
gg_to_npc.ggtikzCanvas <- function(self, coord, panelx, panely) {
    ranges <- get_panel_range(self, panelx, panely)

    coords <- c(
        (coord[1] - ranges$x[1]) / diff(range(ranges$x)),
        (coord[2] - ranges$y[1]) / diff(range(ranges$y))
    )

    return(coords)
}


#' @export
get_refpoints.ggtikzCanvas <- function(self, ggtikzAnnotation) {
    # Store coordinates for extreme points of reference frame

    reference <- ggtikzAnnotation$reference
    panelx <- ggtikzAnnotation$panelx
    panely <- ggtikzAnnotation$panely
    .mult <- ggtikzAnnotation$.mult

    # Multiply unit vector to compensate rounding error with small steps because tikzDevice
    # limits its output to 2 decimal places.
    # Coordinates are stored with the explicit calculation, thus letting
    # tikz handle rounding.

    # Origin (bottom left)
    p00s <- list()
    # Extent (top right)
    p11s <- list()

    p00s$plot <- c(0, 0)
    p11s$plot <- c(1*.mult, 1*.mult)

    ranges <- get_panel_range(self, panelx, panely)
    p00s$panel <- gg_to_npc(self, c(ranges$x[1], ranges$y[1]), panelx, panely)
    p11s$panel <- gg_to_npc(self, c(ranges$x[2], ranges$y[2]), panelx, panely)
    p00s$panel <- p00s$panel * c(.mult, .mult)
    p11s$panel <- p11s$panel * c(.mult, .mult)

    p00s$data <- gg_to_npc(self, c(0, 0), panelx, panely)
    p11s$data <- gg_to_npc(self, c(1*.mult, 1*.mult), panelx, panely)

    p00 <- c(p00s[[reference["x"]]][1], p00s[[reference["y"]]][2])
    p11 <- c(p11s[[reference["x"]]][1], p11s[[reference["y"]]][2])

    refs <- list(p00 = p00, p11 = p11)

    return(refs)
}


#' @export
get_panel_index.ggtikzCanvas <- function(self, panelx, panely) {
    idx <- which(self$panel_info$COL == panelx & self$panel_info$ROW == panely)
    if (length(idx) == 0)
        stop(sprintf("Panel at (%d,%d) is not available", panelx, panely))

    return(idx)
}


#' @export
get_panel_viewport_name.ggtikzCanvas <- function(self, panelx, panely) {
    idx <- get_panel_index(self, panelx, panely)
    panel <- self$panel_info[idx,]
    vp_name <- sprintf("%s.%d-%d-%d-%d", panel$name, panel$t, panel$r, panel$b, panel$l)

    return(vp_name)
}


#' @export
activate_panel.ggtikzCanvas <- function(self, panelx, panely) {
    vp_name <- get_panel_viewport_name(self, panelx, panely)
    grid::seekViewport(vp_name)
}



#' @export
add_annotation_viewport.ggtikzCanvas <- function(self, ggtikzAnnotation) {
    reference <- ggtikzAnnotation$reference
    panelx <- ggtikzAnnotation$panelx
    panely <- ggtikzAnnotation$panely
    clip <- ggtikzAnnotation$clip
    if (all(reference == "plot")) {
        # activate the root viewport
        grid::upViewport(0)
    } else {
        # activate the panel viewport
        tryCatch(
            activate_panel(self, panelx, panely),
            error = function(e) {
                stop("Could not activate the plot panel. Did you forget to print the plot to the device before printing annotations?")
            }
        )
    }
    vp_name <- get_annotation_name(ggtikzAnnotation)
    vp <- grid::viewport(width=1, height=1, name=vp_name, clip=clip)
    grid::pushViewport(vp)

    return(vp_name)
}


#' @export
get_annotation_valid.ggtikzCanvas <- function(self, ggtikzAnnotation) {
    if (!inherits(ggtikzAnnotation, "ggtikzAnnotation"))
        stop("ggtikz annotations must be created with ggtikzAnnotation().")

    if (any(ggtikzAnnotation$reference != "plot")) {
        max_x <- max(self$panel_info$COL, na.rm=TRUE)
        panelx <- ggtikzAnnotation$panelx
        if (panelx > max_x) {
            msg <- sprintf(
                "Annotation wants to be placed in panelx = %d, but the plot only has %d.",
                panelx, max_x)
            stop(msg)
        }
        max_y <- max(self$panel_info$ROW, na.rm=TRUE)
        panely <- ggtikzAnnotation$panely
        if (panely > max_y) {
            msg <- sprintf(
                "Annotation wants to be placed in panely = %d, but the plot only has %d.",
                panely, max_y)
            stop(msg)
        }
    }
}

#' @export
add_annotation.ggtikzCanvas <- function(self, ggtikzAnnotation) {
    get_annotation_valid(self, ggtikzAnnotation)
    ggtikzAnnotation$.id <- length(self$.annotations) + 1
    ggtikzAnnotation <- ggtikzTransform(self, ggtikzAnnotation)
    self$.annotations[[ggtikzAnnotation$.id]] <- ggtikzAnnotation

    return(self)
}

#' @export
"+.ggtikzCanvas" <- function(e1, e2) {
    add_annotation(e1, e2)
}


#' @export
draw_annotation.ggtikzCanvas <- function(self, ggtikzAnnotation) {
    vp_name <- add_annotation_viewport(self, ggtikzAnnotation)
    grid::seekViewport(vp_name)

    refpoints <- get_refpoints(self, ggtikzAnnotation)
    p00 <- refpoints$p00
    p11 <- refpoints$p11

    tikzDevice::grid.tikzCoord(x = p00[1], y = p00[2], name = "p00", units="npc")
    tikzDevice::grid.tikzCoord(x = p11[1], y = p11[2], name = "p11", units="npc")
    tikzDevice::grid.tikzAnnotate("\\coordinate (coord_length) at ($(p11)-(p00)$);")
    # Set up reference coordinate system, taking into account the multiplier
    tikzDevice::grid.tikzAnnotate(paste0("\\path let \\p1 = (coord_length) in coordinate (X) at (\\x1/",ggtikzAnnotation$.mult,",0);"))
    tikzDevice::grid.tikzAnnotate(paste0("\\path let \\p1 = (coord_length) in coordinate (Y) at (0,\\y1/",ggtikzAnnotation$.mult,");"))
    tikzDevice::grid.tikzAnnotate(paste0("
    \\begin{scope}[x=(X), y=(Y), shift=(p00)]
    ", ggtikzAnnotation$tikz_code, "
    \\end{scope}
    "))
}


#' @export
draw_annotations.ggtikzCanvas <- function(self) {
    for (annotation in self$.annotations) {
        draw_annotation(self, annotation)
    }
}
