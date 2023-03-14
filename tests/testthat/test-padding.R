test_that("get_padding_from_elements works", {
    p <- ggplot() + theme(
        panel.border = element_rect(linewidth = 1),
        plot.background = element_rect(linewidth = 2)
    )
    expect_equal(
        get_padding_from_elements(p, "panel.border", "plot.background", "panel.border", "plot.background"),
        grid::unit(c(0.5690, 1.13811, 0.5690, 1.13811), unit = "pt"),
        tolerance = 0.0001)
    expect_equal(
        get_padding_from_elements(p, "plot.background", "plot.background", "panel.border", "panel.border"),
        grid::unit(c(1.13811, 1.13811, 0.5690, 0.5690), unit = "pt"),
        tolerance = 0.0001)
    expect_equal(
        get_padding_from_elements(p, "axis.line.x.top", "plot.background", "panel.border", "panel.border"),
        grid::unit(c(0, 1.13811, 0.5690, 0.5690), unit = "pt"),
        tolerance = 0.0001)
})


test_that("padding is calculated correctly for panels", {
    old <- getOption("tikzLwdUnit")
    # Set option so that output should exactly correspond to input
    options(tikzLwdUnit = 2/.pt)
    on.exit(options(tikzLwdUnit = old))
    p <- ggplot()

    theme1 <- theme(
        panel.border = element_rect(linewidth = 1),
        axis.line.x = element_line(linewidth = 2))
    expect_equal(get_padding_panel(p + theme1), grid::unit(c(2, 1, 2, 1), unit = "pt"))

    theme2 <- theme(
        panel.border = element_rect(linewidth = 1),
        axis.line = element_line(linewidth = 5))
    expect_equal(get_padding_panel(p + theme2), grid::unit(c(5, 5, 5, 5), unit = "pt"))

    theme3 <- theme(
        panel.border = element_rect(linewidth = 1),
        axis.line.y.left = element_line(linewidth = 0))
    expect_equal(get_padding_panel(p + theme3), grid::unit(c(1, 1, 1, 1), unit = "pt"))

    theme4 <- theme(
        panel.border = element_rect(linewidth = 1),
        axis.line.y.left = element_line(linewidth = 5))
    expect_equal(get_padding_panel(p + theme4), grid::unit(c(1, 1, 1, 5), unit = "pt"))

})


test_that("padding is calculated correctly for plots", {
    old <- getOption("tikzLwdUnit")
    # Set option so that output should exactly correspond to input
    options(tikzLwdUnit = 2/.pt)
    on.exit(options(tikzLwdUnit = old))
    p <- ggplot()

    theme1 <- theme(
        plot.background = element_rect(linewidth = 1))
    expect_equal(get_padding_plot(p + theme1), grid::unit(c(1, 1, 1, 1), unit = "pt"))

})
