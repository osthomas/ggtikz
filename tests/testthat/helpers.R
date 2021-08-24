tempTikz <- function(fun, ...) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    tikzDevice::tikz(tmp, ...)
    fun()
    dev.off()
    output <- readLines(tmp)
    return(output)
}


with_mock_requireNamespace <- function(value, code) {
    orig_requireNamespace <- base::requireNamespace
    mock_requireNamespace <- function(...) value
    unlockBinding("requireNamespace", as.environment("package:base"))
    assign("requireNamespace", mock_requireNamespace, "package:base")

    eval(code)

    assign("requireNamespace", orig_requireNamespace, "package:base")
    lockBinding("requireNamespace", as.environment("package:base"))
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
