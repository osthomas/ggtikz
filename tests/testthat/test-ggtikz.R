library(ggplot2)
library(tikzDevice)
library(ggtikz)


test_ggtikz <- function() {
    p <- ggplot()
    ggtikz(p, "\\draw (0,0) -- (1,1);", xy = "plot")
}
test_that("the ggtikz helper works", {
    output <- tempTikz(test_ggtikz)
    expect_match(output[31], "\\\\draw \\(0,0\\) -- \\(1,1\\);")
})


test_ggtikz_nodraw <- function() {
    p <- ggplot()
    c <- ggtikz(p, "\\draw (0,0) -- (1,1);", xy = "panel", panelx=1, panely=1, draw=FALSE)
    print(c)
}
test_that("the ggtikz helper throws an error when no plot is drawn", {
    expect_error(tempTikz(test_ggtikz_nodraw), "Could not activate the plot panel.")
})
