library(ggplot2)
library(ggtikz)
p <- ggplot()

test_that("ggtikz has correct class", {
    obj <- ggtikzCanvas(p)
    expect_equal(class(obj), "ggtikzCanvas")
})


test_that("invalid annotations cannot be added", {
    obj <- ggtikzCanvas(p)
    annot <- ggtikzAnnotation("", panelx = 1, panely = 2)
    expect_error(obj + annot, "wants to be placed in panely = 2")
})


test_that("conversion from data coordinates to npc works", {
    df <- data.frame(x=0:5, y=0:5)
    p <- ggplot(df, aes(x, y)) + coord_cartesian(expand = FALSE)
    obj <- ggtikzCanvas(p)
    expect_equal(gg_to_npc(obj, c(0,0), 1, 1), c(0,0))
    expect_equal(gg_to_npc(obj, c(1,1), 1, 1), c(0.2,0.2))
    expect_equal(gg_to_npc(obj, c(2.5,2.5), 1, 1), c(0.5,0.5))

    p <- ggplot(df, aes(x, y)) +
        scale_x_continuous(expand = expansion(add=2)) +
        scale_y_continuous(expand = expansion(mult=2))
    obj <- ggtikzCanvas(p)
    expect_equal(gg_to_npc(obj, c(-2,-10), 1, 1), c(0,0))
    expect_equal(gg_to_npc(obj, c(2.5,2.5), 1, 1), c(0.5,0.5))
    expect_equal(gg_to_npc(obj, c(7,15), 1, 1), c(1,1))

})


expect_ranges_equal <- function(canvas, panelx, panely, xrange, yrange) {
    ranges <- get_panel_range(canvas, panelx, panely)
    expect_equal(ranges$x, xrange)
    expect_equal(ranges$y, yrange)
}

test_that("panel axis ranges are returned correctly", {
    df <- data.frame(x=1:5, y=6:10)
    p <- ggplot(df, aes(x, y)) + coord_cartesian(expand = FALSE)
    expect_ranges_equal(ggtikzCanvas(p), 1, 1, c(1, 5), c(6, 10))

    p <- ggplot(df, aes(x, y)) +
        scale_x_continuous(expand = expansion(add=1)) +
        scale_y_continuous(expand = expansion(add=1))
    expect_ranges_equal(ggtikzCanvas(p), 1, 1, c(0, 6), c(5, 11))

    p <- ggplot(df, aes(x, y)) + facet_wrap(~x, scales="free") + coord_cartesian(expand=FALSE)
    expect_ranges_equal(ggtikzCanvas(p), 1, 1, c(1, 1), c(6, 6))
    expect_ranges_equal(ggtikzCanvas(p), 2, 1, c(2, 2), c(7, 7))
})


test_that("panel transformations are returned correctly", {
    p <- ggplot() + scale_x_continuous(trans="log10")
    canvas <- ggtikzCanvas(p)
    transforms <- get_panel_transforms(canvas, 1, 1)
    expect_equal(transforms$x(10), 1)
    expect_equal(transforms$x(100), 2)
    expect_equal(transforms$y(10), 10)
    expect_equal(transforms$y(100), 100)

    p <- ggplot() + scale_y_continuous(trans="log10")
    canvas <- ggtikzCanvas(p)
    transforms <- get_panel_transforms(canvas, 1, 1)
    expect_equal(transforms$x(10), 10)
    expect_equal(transforms$x(100), 100)
    expect_equal(transforms$y(10), 1)
    expect_equal(transforms$y(100), 2)
})


expect_ref_equal <- function(annotation, expect_p00, expect_p11) {
    df <- data.frame(x=0:5, y=0:5)
    p <- ggplot(df, aes(x, y)) + coord_cartesian(expand=FALSE)

    obj <- ggtikzCanvas(p)
    refs <- get_refpoints(obj, annotation)
    p00 <- refs$p00
    p11 <- refs$p11
    expect_equal(p00, expect_p00)
    expect_equal(p11, expect_p11)
}

test_that("reference points are calculated correctly", {
    obj <- ggtikzCanvas(p)
    expect_ref_equal(ggtikzAnnotation("", xy = "plot"),  c(0,0), c(5,5))
    expect_ref_equal(ggtikzAnnotation("", xy = "panel", panelx=1, panely=1), c(0,0), c(50,50))
    expect_ref_equal(ggtikzAnnotation("", xy = "data", panelx=1, panely=1), c(0,0), c(10,10))
    expect_ref_equal(ggtikzAnnotation("", x = "data", y="panel", panelx=1, panely=1), c(0,0), c(10,50))
    expect_ref_equal(ggtikzAnnotation("", x = "panel", y="data", panelx=1, panely=1), c(0,0), c(50,10))
})


vp_from_p <- function(p, panelx, panely) {
    canvas <- ggtikzCanvas(p)
    vp_name <- get_panel_viewport_name(canvas, panelx, panely)
    return(vp_name)
}
expect_vp_name_equal <- function(p, panelx, panely, expect) {
    vp_name <- vp_from_p(p, panelx, panely)
    expect_equal(vp_name, expect)
}

test_that("Panel viewport names are determined correctly", {
    df <- data.frame(a = 3:1, b = 6:4, x=1:3, y=1:3)
    p <- ggplot(df, aes(x, y))

    p_wrap1 <- p + facet_wrap(~a, as.table = TRUE)
    expect_vp_name_equal(p_wrap1, 1, 1, "panel-1-1.8-5-8-5")
    expect_vp_name_equal(p_wrap1, 2, 1, "panel-2-1.8-9-8-9")
    expect_vp_name_equal(p_wrap1, 3, 1, "panel-3-1.8-13-8-13")
    expect_error(vp_from_p(p_wrap1, 4, 1), "Panel at \\(4,1\\) is not available")

    p_wrap2 <- p + facet_wrap(~a, ncol=2, as.table = FALSE)
    expect_vp_name_equal(p_wrap2, 1, 1, "panel-1-1.8-5-8-5")
    expect_vp_name_equal(p_wrap2, 2, 2, "panel-2-2.13-9-13-9")
    expect_error(vp_from_p(p_wrap2, 2, 1), "Panel at \\(2,1\\) is not available")

    p_grid1 <- p + facet_grid(a~b, as.table = TRUE)
    expect_vp_name_equal(p_grid1, 1, 1, "panel-1-1.8-5-8-5")
    expect_vp_name_equal(p_grid1, 2, 1, "panel-1-2.8-7-8-7")
    expect_vp_name_equal(p_grid1, 1, 2, "panel-2-1.10-5-10-5")
    expect_vp_name_equal(p_grid1, 2, 2, "panel-2-2.10-7-10-7")
    expect_vp_name_equal(p_grid1, 1, 3, "panel-3-1.12-5-12-5")
    expect_vp_name_equal(p_grid1, 2, 3, "panel-3-2.12-7-12-7")

    p_grid2 <- p + facet_grid(a~b, as.table = FALSE)
    expect_vp_name_equal(p_grid1, 1, 1, "panel-1-1.8-5-8-5")
    expect_vp_name_equal(p_grid1, 2, 1, "panel-1-2.8-7-8-7")
    expect_vp_name_equal(p_grid1, 1, 2, "panel-2-1.10-5-10-5")
    expect_vp_name_equal(p_grid1, 2, 2, "panel-2-2.10-7-10-7")
    expect_vp_name_equal(p_grid1, 1, 3, "panel-3-1.12-5-12-5")
    expect_vp_name_equal(p_grid1, 2, 3, "panel-3-2.12-7-12-7")
})


expect_cur_vp_equal <- function(expect) {
    expect_equal(grid::current.viewport()$name, expect)
}

check_activated_vp <- function(p, panelx, panely, expect) {
    canvas <- ggtikzCanvas(p)
    pdf(NULL)
    print(p)
    activate_panel(canvas, panelx, panely)
    expect_cur_vp_equal(expect)
    dev.off()
}

test_that("Panel viewports can be activated", {
    df <- data.frame(a = 3:1, b = 6:4, x=1:3, y=1:3)
    p <- ggplot(df, aes(x, y))
    p_grid1 <- p + facet_grid(a~b, as.table = TRUE)
    check_activated_vp(p_grid1, 1, 1, "panel-1-1.8-5-8-5")
    check_activated_vp(p_grid1, 2, 3, "panel-3-2.12-7-12-7")
})


check_added_vp <- function(p, canvas, n, expect) {
    pdf(NULL)
    print(p)
    vp_name <- add_annotation_viewport(canvas, canvas$.annotations[[n]])
    expect_cur_vp_equal(expect)
    dev.off()
    expect_equal(vp_name, expect)
}

test_that("annotation viewports are added correctly", {
    p <- ggplot()
    canvas <- ggtikzCanvas(p)
    a1 <- ggtikzAnnotation("", xy = "plot")
    canvas <- canvas + a1
    check_added_vp(p, canvas, 1, "ggtikzannotation_1")

    a2 <- ggtikzAnnotation("", xy = "data", panelx=1, panely=1)
    canvas <- canvas + a2
    check_added_vp(p, canvas, 2, "ggtikzannotation_2")
})


test_that("invalid annotations are rejected", {
    canvas <- ggtikzCanvas(p)

    expect_error(
        canvas + list(),
        "must be created with ggtikzAnnotation")
    expect_error(
        canvas + ggtikzAnnotation("", xy = "data", panelx = 100, panely=1),
        "wants to be placed in panelx = 100, but")
    expect_error(
        canvas + ggtikzAnnotation("", xy = "data", panelx = 1, panely=100),
        "wants to be placed in panely = 100, but")
})



testfun_ggtikz <- function() {
    canvas <- ggtikzCanvas(p)
    annot <- ggtikzAnnotation("\\draw (0,0) -- (1,1);", xy = "plot")
    print(p)
    print(canvas + annot)
}
test_that("drawing annotations does not fail", {
    output <- tempTikz(testfun_ggtikz)
    expect_match(output[28], "\\\\draw \\(0,0\\) -- \\(1,1\\);")
})
