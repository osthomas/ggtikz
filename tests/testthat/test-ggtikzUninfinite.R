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


test_that("padding is calculated correctly", {
    old <- getOption("tikzLwdUnit")
    # Set option so that output should exactly correspond to input
    options(tikzLwdUnit = 2 * 1/.pt)
    on.exit(options(tikzLwdUnit = old))
    p <- ggplot()

    theme1 <- theme(
        panel.border = element_rect(size = 1),
        axis.line.x = element_line(size = 2))
    expect_equal(get_padding(p + theme1), c(t = "2pt", r = "1pt", b = "2pt", l = "1pt"))

    theme2 <- theme(
        panel.border = element_rect(size = 1),
        axis.line = element_line(size = 5))
    expect_equal(get_padding(p + theme2), c(t = "5pt", r = "5pt", b = "5pt", l = "5pt"))

    theme3 <- theme(
        panel.border = element_rect(size = 1),
        axis.line.y.left = element_line(size = 0))
    expect_equal(get_padding(p + theme3), c(t = "1pt", r = "1pt", b = "1pt", l = "1pt"))

    theme4 <- theme(
        panel.border = element_rect(size = 1),
        axis.line.y.left = element_line(size = 5))
    expect_equal(get_padding(p + theme4), c(t = "1pt", r = "1pt", b = "1pt", l = "5pt"))

})


test_that("padding is adjusted correctly", {
    pad <- c(t = "1pt", r = "2pt", b = "3pt", l = "4pt")
    expect_equal(adjust_padding(c(0, 0), pad), c("0", "0"))
    expect_equal(adjust_padding(c(Inf, 0), pad), c("-2pt", "0"))
    expect_equal(adjust_padding(c(-Inf, 0), pad), c("4pt", "0"))
    expect_equal(adjust_padding(c(0, Inf), pad), c("0", "-1pt"))
    expect_equal(adjust_padding(c(0, -Inf), pad), c("0", "3pt"))
    expect_equal(adjust_padding(c(Inf, Inf), pad), c("-2pt", "-1pt"))
    expect_equal(adjust_padding(c(-Inf, Inf), pad), c("4pt", "-1pt"))
    expect_equal(adjust_padding(c(Inf, -Inf), pad), c("-2pt", "3pt"))
    expect_equal(adjust_padding(c(-Inf, -Inf), pad), c("4pt", "3pt"))
})

test_that("replacement of infinite values works", {
    x <- c(-1, 1)
    y <- c(-2, 2)
    pad <- c(t = "1pt", r = "2pt", b = "3pt", l = "4pt")

    expect_equal(uninfinite_coord("(0,0)", x, y, pad), "(0,0)")
    expect_equal(uninfinite_coord("(5,1cm)", x, y, pad), "(5,1cm)")
    expect_equal(uninfinite_coord("(-Inf,0)", x, y, pad), "($(-1,0)+(4pt,0)$)")
    expect_equal(uninfinite_coord("(-Inf,1cm)", x, y, pad), "($(-1,1cm)+(4pt,0)$)")
    expect_equal(uninfinite_coord("(Inf,Inf)", x, y, pad), "($(1,2)+(-2pt,-1pt)$)")
})


expect_uninfinite_equal <- function(canvas, annotation, expect) {
    result <- ggtikzUninfinite(canvas, annotation)$tikz_code
    expect_equal(result, expect)
}
test_that("removal of Inf from annotations works in a linear scale", {
    c1 <- canvas_lin()
    annot_plot <- ggtikzAnnotation("\\draw (-Inf,Inf) -- (1,1);", xy = "plot")
    expect_uninfinite_equal(c1, annot_plot, "\\draw ($(0,1)+(0pt,-0pt)$) -- (1,1);")

    annot_data <- ggtikzAnnotation("\\draw (-Inf,Inf) -- (Inf,-Inf);", xy = "data", panelx=1, panely=1)
    expect_uninfinite_equal(c1, annot_data, "\\draw ($(9.225,472)+(0pt,-0pt)$) -- ($(35.075,71.1)+(-0pt,0pt)$);")

    c1$p <- c1$p + theme(panel.border = element_rect(size=5))
    expect_uninfinite_equal(c1, annot_data, "\\draw ($(9.225,472)+(2.85pt,-2.85pt)$) -- ($(35.075,71.1)+(-2.85pt,2.85pt)$);")
})


test_that("removal of Inf from annotations works in a log scale", {
    c1 <- canvas_y_log10()
    annot_plot <- ggtikzAnnotation("\\draw (-Inf,Inf) -- (1,1);", xy = "plot")
    expect_uninfinite_equal(c1, annot_plot, "\\draw ($(0,1)+(0pt,-0pt)$) -- (1,1);")

    annot_data <- ggtikzAnnotation("\\draw (-Inf,Inf) -- (Inf,-Inf);", xy = "data", panelx=1, panely=1)
    expect_uninfinite_equal(c1, annot_data, "\\draw ($(9.225,2.67394199863409)+(0pt,-0pt)$) -- ($(35.075,1.85186960072977)+(-0pt,0pt)$);")

    c1$p <- c1$p + theme(panel.border = element_rect(size=5))
    expect_uninfinite_equal(c1, annot_data, "\\draw ($(9.225,2.67394199863409)+(2.85pt,-2.85pt)$) -- ($(35.075,1.85186960072977)+(-2.85pt,2.85pt)$);")
})


test_that("ggtikzUnfinite raises an error only when replacements are necessary and stringr is missing", {
    c1 <- canvas_lin()
    annot_uninf <- ggtikzAnnotation("", xy = "plot", replace_inf = TRUE)
    annot_nouninf <- ggtikzAnnotation("", xy = "plot", replace_inf = FALSE)

    with_mock_requireNamespace(FALSE,
        expect_error(ggtikzUninfinite(c1, annot_uninf), "requires the `stringr` package")
    )
    with_mock_requireNamespace(FALSE,
        expect_error(ggtikzUninfinite(c1, annot_nouninf), NA)
    )
    with_mock_requireNamespace(TRUE,
        expect_error(ggtikzUninfinite(c1, annot_uninf), NA)
    )
    with_mock_requireNamespace(TRUE,
        expect_error(ggtikzUninfinite(c1, annot_nouninf), NA)
    )
})
