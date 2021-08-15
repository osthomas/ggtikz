library(tikzDevice)

tempTikz <- function(fun, ...) {
    tmp <- tempfile()
    print(tmp)
    on.exit(unlink(tmp))
    tikz(tmp, ...)
    fun()
    dev.off()
    output <- readLines(tmp)
    return(output)
}
