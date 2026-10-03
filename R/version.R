#' The installed version of islandbrand
#'
#' `DESCRIPTION` is the single source of the version; this reads it back so
#' nothing in the package carries a second copy that can drift.
#'
#' @return A `package_version` object.
#' @export
#'
#' @examples
#' islh_version()
islh_version <- function() {
  utils::packageVersion("islandbrand")
}
