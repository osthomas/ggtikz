library(ggplot2)
library(ggtikz)
p <- ggplot()

test_that("annotations cannot be constructed faultily", {
    expect_warning(ggtikzAnnotation("", xy="plot", panelx=1, panely=2), "ignored for plot reference")
    expect_error(ggtikzAnnotation("", xy="data", panelx=1), "must be given")
    expect_error(ggtikzAnnotation("", xy="data", panely=1), "must be given")
})


expect_reference_equal <- function(annotation, reference) {
    expect_equal(annotation$reference, reference)
}


test_that("annotation can be constructed correctly", {
    expect_reference_equal(ggtikzAnnotation("", xy="plot"), c(x="plot", y="plot"))
    expect_reference_equal(ggtikzAnnotation("", xy="panel", panelx=1, panely=1), c(x="panel", y="panel"))
    expect_reference_equal(ggtikzAnnotation("", x="panel", y="data", panelx=1, panely=1), c(x="panel", y="data"))
    expect_reference_equal(ggtikzAnnotation("", x="panel", y="data", xy="data", panelx=1, panely=1), c(x="data", y="data"))
})


test_that("annotation can get a name after being added to a canvas", {
    canvas <- ggtikzCanvas(p)
    annot <- ggtikzAnnotation("", xy = "plot")
    expect_error(get_annotation_name(annot), "name can only be given once the")

    canvas <- canvas + annot + annot
    expect_equal(get_annotation_name(canvas$.annotations[[1]]), "ggtikzannotation_1")
    expect_equal(get_annotation_name(canvas$.annotations[[2]]), "ggtikzannotation_2")
})



test_that("a warning is issued when panels are specified with a plot reference and the default is set correctly.", {
    expect_warning(
        both <- ggtikzAnnotation("", xy = "plot", panelx = 1, panely = 1),
        "ignored for plot reference")
    expect_equal(both$panelx, 1)
    expect_equal(both$panely, 1)

    expect_warning(
        one1 <- ggtikzAnnotation("", xy = "plot", panelx = 1),
        "ignored for plot reference")
    expect_equal(one1$panelx, 1)
    expect_equal(one1$panely, 1)

    expect_warning(
        one2 <- ggtikzAnnotation("", xy = "plot", panely = 1),
        "ignored for plot reference")
    expect_equal(one2$panelx, 1)
    expect_equal(one2$panely, 1)
})
