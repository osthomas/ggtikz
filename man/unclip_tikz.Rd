% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unclip.R
\name{unclip_tikz}
\alias{unclip_tikz}
\title{Unclip a plot produced by the \code{tikzDevice}.}
\usage{
unclip_tikz(fpath)
}
\arguments{
\item{fpath}{Path to the tikz file}
}
\value{
Called for side effect.
The file at \code{fpath} is edited and overwritten.
}
\description{
By default, plots produced with the tikzDevice are clipped to the plot area,
which also clips ggtikzAnnotations extending beyond the plot boundaries. This
function removes the 'clip' and 'use as bounding box' options in a tikz file.
}
\details{
This function can be used for manual post-processing, however,
see \code{\link{set_ggtikz_unclip_hook}} to set the corresponding knitr hook.
}
\seealso{
\code{\link{set_ggtikz_unclip_hook}} to set the knitr hook.
}
