test_that("infinite coordinate values are discretized correctly", {
    x <- c(-1, 1)
    y <- c(-2, 2)
    expect_equal(discretize(c(0, 0), x, y), c(0, 0))
    expect_equal(discretize(c(-10, 10), x, y), c(-10, 10))
    expect_equal(discretize(c(-Inf, 0), x, y), c(-1, 0))
    expect_equal(discretize(c(Inf, 0), x, y), c(1, 0))
    expect_equal(discretize(c(0, -Inf), x, y), c(0, -2))
    expect_equal(discretize(c(0, Inf), x, y), c(0, 2))
    expect_equal(discretize(c(Inf, Inf), x, y), c(1, 2))
    expect_equal(discretize(c(-Inf, -Inf), x, y), c(-1, -2))
    expect_equal(discretize(c(Inf, -Inf), x, y), c(1, -2))
    expect_equal(discretize(c(-Inf, Inf), x, y), c(-1, 2))
})


test_that("replacement of infinite values works", {
    x <- c(-1, 1)
    y <- c(-2, 2)

    expect_equal(uninfinite_coord("(0,0)", x, y), "(0,0)")
    expect_equal(uninfinite_coord("(5,1cm)", x, y), "(5,1cm)")
    expect_equal(uninfinite_coord("(-Inf,0)", x, y), "(-1,0)")
    expect_equal(uninfinite_coord("(-Inf,1cm)", x, y), "(-1,1cm)")
    expect_equal(uninfinite_coord("(Inf,Inf)", x, y), "(1,2)")
})


expect_uninfinite_equal <- function(canvas, annotation, expect) {
    result <- ggtikzUninfinite(canvas, annotation)$tikz_code
    expect_equal(result, expect)
}
test_that("removal of Inf from annotations works in a linear scale", {
    c1 <- canvas_lin()
    annot_plot <- ggtikzAnnotation("\\draw (-Inf,Inf) -- (1,1);", xy = "plot")
    expect_uninfinite_equal(c1, annot_plot, "\\draw (0,1) -- (1,1);")

    annot_data <- ggtikzAnnotation("\\draw (-Inf,Inf) -- (Inf,-Inf);", xy = "data", panelx=1, panely=1)
    expect_uninfinite_equal(c1, annot_data, "\\draw (9.225,472) -- (35.075,71.1);")

    c1$p <- c1$p + theme(panel.border = element_rect(linewidth = 5))
    expect_uninfinite_equal(c1, annot_data, "\\draw (9.225,472) -- (35.075,71.1);")
})


test_that("removal of Inf from annotations works in a log scale", {
    c1 <- canvas_y_log10()
    annot_plot <- ggtikzAnnotation("\\draw (-Inf,Inf) -- (1,1);", xy = "plot")
    expect_uninfinite_equal(c1, annot_plot, "\\draw (0,1) -- (1,1);")

    annot_data <- ggtikzAnnotation("\\draw (-Inf,Inf) -- (Inf,-Inf);", xy = "data", panelx=1, panely=1)
    expect_uninfinite_equal(c1, annot_data, "\\draw (9.225,2.67394199863409) -- (35.075,1.85186960072977);")

    c1$p <- c1$p + theme(panel.border = element_rect(linewidth  = 5))
    expect_uninfinite_equal(c1, annot_data, "\\draw (9.225,2.67394199863409) -- (35.075,1.85186960072977);")
})
