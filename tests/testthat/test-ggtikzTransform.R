library(ggplot2)
library(ggtikz)
plus1 <- function(x) x + 1
plus2 <- function(x) x + 2


test_that("numeric coordinate components are transformed", {
    expect_equal(try_transform("1", plus1), 2)
    expect_equal(try_transform("1", plus2), 3)
})


test_that("transformation to infinite and NA values raise an errors", {
    expect_error(try_transform("-1", log), "value -1 could not be transformed")
    expect_error(try_transform("0", log), "value 0 could not be transformed")
})


test_that("non-numeric coordinate components are not transformed", {
    expect_equal(try_transform("1in", plus1), "1in")
    expect_equal(try_transform("2 cm", plus2), "2 cm")
})


test_that("explicitly input infinite coordinate components are not transformed", {
    expect_equal(try_transform("inf", plus1), "inf")
    expect_equal(try_transform("-inf", plus1), "-inf")
    expect_equal(try_transform("Inf", plus1), "Inf")
    expect_equal(try_transform("-Inf", plus1), "-Inf")
})


test_that("a single coordinate can be transformed", {
    expect_equal(transform_coord("(0,0)", plus1, plus1), "(1,1)")
    expect_equal(transform_coord("(0,0)", plus1, plus2), "(1,2)")
    expect_equal(transform_coord("(0,0)", plus2, plus1), "(2,1)")
    expect_equal(transform_coord("(0 , 0 )", plus1, plus1), "(1,1)")
    expect_equal(transform_coord("(  0, 0 )", plus1, plus1), "(1,1)")
})


test_that("multiple coordinates can be transformed", {
    coords <- "(0,0) -- (1,1) -- (2,2)"
    expect_equal(transform_tikz(coords, plus1, plus1), "(1,1) -- (2,2) -- (3,3)")
})


test_that("multi-line tikz code can be transformed", {
    tikz_code <- "
        \\draw (0,0) -- (50,50) -- cycle;
        \\node at (5,5) {Text};
    "
    expect <- "
        \\draw (1,2) -- (51,52) -- cycle;
        \\node at (6,7) {Text};
    "
    expect_equal(transform_tikz(tikz_code, plus1, plus2), expect)
})


test_that("coordinates intermixed with radii can be transformed", {
    input  <- "\\draw (0,20) circle (20mm) node at (30,40) to (2,3)(4,5) (3mm) to (3,4)"
    expect <- "\\draw (1,21) circle (20mm) node at (31,41) to (3,4)(5,6) (3mm) to (4,5)"
    result <- transform_tikz(input, plus1, plus1)
    expect_equal(result, expect)
})


expect_transformed_equal <- function(canvas, annotation, expect) {
    transformed <- ggtikzTransform(canvas, annotation)
    expect_equal(transformed$tikz_code, expect)
}
test_that("tikz_code in annotations is transformed for data reference frames", {
    canvas <- canvas_y_log10()
    annotation <- ggtikzAnnotation("\\draw (10,10) -- (100,100);", xy="data", panelx=1, panely=1)
    expect_transformed_equal(canvas, annotation, "\\draw (10,1) -- (100,2);")
})

test_that("tikz_code in annotations is transformed correctly for discrete scales", {
    canvas <- canvas_x_discrete()
    annotation <- ggtikzAnnotation("\\draw (1,1) -- (2,2);", xy="data", panelx=1, panely=1)
    expect_transformed_equal(canvas, annotation, "\\draw (1,1) -- (2,2);")
})


test_that("tikz_code in annotations is not transformed for panel reference frames", {
    canvas <- canvas_y_log10()
    annotation <- ggtikzAnnotation("\\draw (0.5,0.5) -- (0.5,0.5);", xy="panel", panelx=1, panely=1)
    expect_transformed_equal(canvas, annotation, "\\draw (0.5,0.5) -- (0.5,0.5);")
})


test_that("tikz_code in annotations is not transformed for plot reference frames", {
    canvas <- canvas_y_log10()
    annotation <- ggtikzAnnotation("\\draw (0.5,0.5) -- (0.5,0.5);", xy="plot")
    expect_transformed_equal(canvas, annotation, "\\draw (0.5,0.5) -- (0.5,0.5);")
})


test_that("tikz_code in annotations is transformed correctly for mixed references", {
    canvas <- canvas_y_log10()
    annotation <- ggtikzAnnotation("\\draw (0.5,10) -- (0.5,100);", x="panel", y="data", panelx=1, panely=1)
    expect_transformed_equal(canvas, annotation, "\\draw (0.5,1) -- (0.5,2);")
})

test_that("tikz_code in annotations is transformed only once", {
    canvas <- canvas_y_log10()
    annotation <- ggtikzAnnotation("\\draw (10,10) -- (100,100);", xy="data", panelx=1, panely=1)
    expect_false(annotation$.transformed)
    # Adding annotation transforms ...
    canvas <- canvas + annotation
    annotation_tf <- canvas$.annotations[[1]]
    expect_true(annotation_tf$.transformed)
    expect_equal(annotation_tf$tikz_code, "\\draw (10,1) -- (100,2);")
    # Try to transform again ...
    annotation_tf2 <- ggtikzTransform(canvas, annotation_tf)
    expect_equal(annotation_tf2$tikz_code, "\\draw (10,1) -- (100,2);")
})
