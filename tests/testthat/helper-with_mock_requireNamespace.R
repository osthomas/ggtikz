with_mock_requireNamespace <- function(value, code) {
    orig_requireNamespace <- base::requireNamespace
    mock_requireNamespace <- function(...) value
    unlockBinding("requireNamespace", as.environment("package:base"))
    assign("requireNamespace", mock_requireNamespace, "package:base")

    eval(code)

    assign("requireNamespace", orig_requireNamespace, "package:base")
    lockBinding("requireNamespace", as.environment("package:base"))
}
