library(ggplot2)

tempTikz <- function(fun, ...) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    tikzDevice::tikz(tmp, ...)
    fun()
    dev.off()
    output <- readLines(tmp)
    return(output)
}


with_mock_require <- function(value, code) {
    orig_require <- base::require
    mock_require <- function(...) value
    unlockBinding("require", as.environment("package:base"))
    assign("require", mock_require, "package:base")

    eval(code)

    assign("require", orig_require, "package:base")
    lockBinding("require", as.environment("package:base"))
}


canvas_y_log10 <- function() {
    p <- ggplot(mtcars, aes(mpg,disp)) + geom_point() + scale_y_continuous(trans="log10", expand=expansion(0, 0))
    canvas <- ggtikzCanvas(p)
    return(canvas)
}


canvas_lin <- function() {
    p <- ggplot(mtcars, aes(mpg,disp)) + geom_point() + scale_y_continuous(expand=expansion(0, 0))
    canvas <- ggtikzCanvas(p)
    return(canvas)
}

canvas_x_discrete <- function() {
    p <- ggplot(mtcars, aes(as.factor(cyl), mpg)) + scale_x_discrete() + geom_point()
    canvas <- ggtikzCanvas(p)
    return(canvas)
}
