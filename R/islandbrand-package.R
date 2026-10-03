#' @keywords internal
#' @importFrom rlang .data
#' @importFrom utils packageVersion
"_PACKAGE"

# Column names used inside `ggplot2::aes()` in the example and gallery
# helpers. They are evaluated in the data, not in the package namespace, but
# R CMD check cannot tell the difference.
utils::globalVariables(c("cyl", "mpg", "value", "wt", "x", "y"))
