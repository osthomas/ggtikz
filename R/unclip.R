#' Unclip a plot produced by the `tikzDevice`.
#'
#' By default, plots produced with the tikzDevice are clipped to the plot area,
#' which also clips ggtikzAnnotations extending beyond the plot boundaries. This
#' function removes the 'clip' and 'use as bounding box' options in a tikz file.
#'
#' This function can be used for manual post-processing, however,
#' see \code{\link{set_ggtikz_unclip_hook}} to set the corresponding knitr hook.
#'
#' @param fpath Path to the tikz file
#'
#' @returns Called for side effect.
#'  The file at `fpath` is edited and overwritten.
#'
#' @seealso \code{\link{set_ggtikz_unclip_hook}} to set the knitr hook.
#'
#' @export
unclip_tikz <- function(fpath) {
    lines <- readLines(fpath)
    new <- lines

    # Remove definition of explicit bounding box: use space as required
    bblines <- grep("use as bounding box", lines)
    if (length(bblines) > 0) {
        new <- new[-bblines]
    }

    # The first 'clip' command we find is the one clipping the whole plot
    cliplines <- grep("clip", lines)
    if (length(cliplines) > 0) {
        plotclip <- lines[min(cliplines)]
        plotcliplines <- grep(plotclip, new, fixed=TRUE)
        new <- new[-plotcliplines]
    }

    writeLines(new, fpath)
}


#' knitr hook to remove clipping from plots produced with the tikzDevice.
#'
#' Note that the chunk options `unclip = TRUE` and `external = FALSE`must be set
#' for the hook to come into effect!
#'
#' @param before see \code{\link[knitr]{knit_hooks}}
#' @param options see \code{\link[knitr]{knit_hooks}}
#'
#' @returns Called for side effect.
#'  The files containing tikz plots are edited and overwritten.
#'
#' @seealso \code{\link{set_ggtikz_unclip_hook}} to set the knitr hook.
#' @seealso \code{\link{unclip_tikz}}, the workhorse function for this hook.
#'
#' @export
unclip <- function(before, options) {
    if (!before & options$unclip != FALSE) {
        plot_files <- unique(knitr::opts_knit$get("plot_files"))
        tikz_files <- grep("[.]tex", plot_files, ignore.case = TRUE, value = TRUE)
        for (tikz_file in tikz_files) {
            unclip_tikz(tikz_file)
        }
    }
}


#' Unclip plots produced by the `tikzDevice`.
#'
#' By default, plots produced with the tikzDevice are clipped to the plot area,
#' which also clips ggtikzAnnotations extending beyond the plot boundaries. This
#' function removes the 'clip' and 'use as bounding box' options in a tikz file.
#'
#' @returns Called for side effects - the `unclip` knitr hook is set or unset,
#'  respectively.
#'
#' @seealso \code{\link{unclip}}, the hook that is being set.
#'
#' @export
set_ggtikz_unclip_hook <- function() {
    knitr::knit_hooks$set(unclip=unclip)
}

#' @rdname set_ggtikz_unclip_hook
#' @export
unset_ggtikz_unclip_hook <- function() {
    knitr::knit_hooks$set(unclip=NULL)
}
