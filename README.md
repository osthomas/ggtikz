
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggtikz

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggtikz)](https://CRAN.R-project.org/package=ggtikz)
[![codecov](https://codecov.io/gh/osthomas/ggtikz/branch/main/graph/badge.svg?token=0LPNGPFO5Z)](https://codecov.io/gh/osthomas/ggtikz)
[![R-CMD-check](https://github.com/osthomas/ggtikz/workflows/R-CMD-check/badge.svg)](https://github.com/osthomas/ggtikz/actions)
<!-- badges: end -->

ggtikz allows you to annotate plots created using
[ggplot2](https://ggplot2.tidyverse.org/) with arbitrary TikZ code when
rendering them with the
[tikzDevice](https://CRAN.R-project.org/package=tikzDevice). The
annotations can be made using data coordinates, or with coordinates
relative to a specified panel or the whole plot.

Plots with multiple panels (via `facet_grid()` or `facet_wrap()`) are
supported.

For a few examples, see the [examples
vignette](https://github.com/osthomas/ggtikz/blob/main/doc/examples.pdf).

## Installation

You can install the latest ggtikz release from CRAN with:

``` r
install.packages("ggtikz")
```

Or get the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("osthomas/ggtikz", ref = "devel")
```

## New in Devel

[![codecov
(devel)](https://codecov.io/gh/osthomas/ggtikz/branch/devel/graph/badge.svg?token=0LPNGPFO5Z)](https://codecov.io/gh/osthomas/ggtikz)
[![R-CMD-check
(devel)](https://github.com/osthomas/ggtikz/workflows/R-CMD-check/badge.svg?branch=devel)](https://github.com/osthomas/ggtikz/actions)

The TikZ coordinates used with transformed scales can no be
automatically transformed with the `transform = TRUE` (now the default)
option for `ggtikzAnnotation` – [see examples in the devel
branch](https://github.com/osthomas/ggtikz/tree/devel).

## Basic Usage

1.  Create a ggplot.
2.  Add annotations with `ggtikz()`.

``` r
library(ggplot2)
library(ggtikz)

p <- ggplot(mtcars, aes(disp, mpg)) + geom_point() # 1.
## tikz("plot.tikz")
ggtikz(p,
    "\\fill[red] (0.5,0.5) circle (5mm);",
    xy = "plot")
## dev.off()
## Render with LaTeX ...
```

<img src="man/figures/README-basic-usage-1.png" width="50%" />

## Advanced Usage

1.  Create a ggplot.
2.  Create a ggtikz canvas from the plot with `ggtikzCanvas()`.
3.  Create ggtikz annotations with `ggtikzAnnotation()`.
4.  Add the annotations to the canvas.
5.  Draw the plot and the annotations using tikzDevice.

``` r
library(ggplot2)
library(ggtikz)

p <- ggplot(mtcars, aes(disp, mpg)) + geom_point()  # 1.
canvas <- ggtikzCanvas(p)                           # 2.
annot1 <- ggtikzAnnotation(                         # 3.
    "
    \\draw (0,0) -- (1,1);
    \\draw (0,1) -- (1,0);
    \\fill[red] (0.5,0.5) circle (5mm);
    ",
    xy = "panel", panelx = 1, panely = 1
)
annot2 <- ggtikzAnnotation(                          # 3.
    "\\draw[<-] (400,20) -- ++(0,3) node[at end, anchor=south] {(400,20)};",
    xy = "data", panelx = 1, panely = 1
)

## tikz("plot.tikz")
p                                                   # 4. + 5.
canvas + annot1 + annot2
## dev.off()
## Render with LaTeX ...
```

<img src="man/figures/README-advanced-usage-1.png" width="50%" />
