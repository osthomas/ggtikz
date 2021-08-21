test_that("unclip_tikz removes clip and use as bounding box", {
    clipped <- readLines(system.file("testdata", "plot_clipped.tex", package="ggtikz"))
    unclipped <- readLines(system.file("testdata", "plot_unclipped.tex", package="ggtikz"))
    tmp <- tempfile()
    on.exit(unlink(tmp))
    writeLines(clipped, tmp)
    unclip_tikz(tmp)
    result <- readLines(tmp)
    expect_equal(result, unclipped)
})


test_that("hook can be set and unset", {
    # Default: not set
    expect_identical(knitr::knit_hooks$get("unclip"), NULL)

    set_ggtikz_unclip_hook()
    expect_identical(knitr::knit_hooks$get("unclip"), unclip)

    unset_ggtikz_unclip_hook()
    expect_identical(knitr::knit_hooks$get("unclip"), NULL)
})


test_that("the hook works", {
    clipped <- readLines(system.file("testdata", "plot_clipped.tex", package="ggtikz"))
    unclipped <- readLines(system.file("testdata", "plot_unclipped.tex", package="ggtikz"))
    tmp <- tempfile(fileext=".tex")
    on.exit(unlink(tmp))
    writeLines(clipped, tmp)
    knitr::opts_knit$set(plot_files = tmp)

    # should only run after chunk execution and with the unclip
    unclip(before = FALSE, options=list(unclip = TRUE))
    result <- readLines(tmp)
    expect_equal(result, unclipped)
})


test_that("the hook only runs after chunk execution and with the `unclip` option", {
    clipped <- readLines(system.file("testdata", "plot_clipped.tex", package="ggtikz"))
    tmp <- tempfile(fileext=".tex")
    on.exit(unlink(tmp))
    writeLines(clipped, tmp)
    knitr::opts_knit$set(plot_files = tmp)
    unclip(before = TRUE, options=list(unclip = TRUE))
    unclip(before = TRUE, options=list(unclip = FALSE))
    unclip(before = FALSE, options=list(unclip = FALSE))
    result <- readLines(tmp)
    expect_equal(result, clipped)
})
