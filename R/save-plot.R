#' Save an Island Health plot with a standard size
#'
#' @param filename Output PNG filename.
#' @param plot Plot to save.
#' @param preset Standard output size: report, slide, or half_width.
#' @param width,height Optional size overrides in inches.
#' @param dpi Optional resolution override.
#' @param bg Background colour.
#' @param ... Additional arguments passed to `ggplot2::ggsave()`.
#'
#' @return The filename, invisibly.
#'
#' @examples
#' \dontshow{assign("font", "", envir = getFromNamespace(".islh_state", "islandbrand"))}
#' if (requireNamespace("ragg", quietly = TRUE)) {
#'   path <- tempfile(fileext = ".png")
#'   islh_save_plot(path, islh_example_plot(), preset = "half_width")
#'   file.exists(path)
#' }
#'
#' @export
islh_save_plot <- function(
  filename,
  plot = ggplot2::last_plot(),
  preset = c("report", "slide", "half_width"),
  width = NULL,
  height = NULL,
  dpi = NULL,
  bg = "white",
  ...
) {
  .islh_require("ragg", "saving standard Island Health plot files")
  preset <- match.arg(preset)

  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    .islh_abort("{.arg filename} must be one file path.")
  }
  if (tolower(tools::file_ext(filename)) != "png") {
    .islh_abort("{.arg filename} must end in {.file .png}.")
  }

  settings <- list(
    report = c(width = 6.5, height = 4.2, dpi = 300),
    slide = c(width = 10, height = 5.625, dpi = 192),
    half_width = c(width = 3.15, height = 3.5, dpi = 300)
  )[[preset]]

  width <- .islh_check_dimension(
    if (is.null(width)) settings[["width"]] else width,
    "width"
  )
  height <- .islh_check_dimension(
    if (is.null(height)) settings[["height"]] else height,
    "height"
  )
  dpi <- .islh_check_dpi(if (is.null(dpi)) settings[["dpi"]] else dpi)

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = ragg::agg_png,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = bg,
    ...
  )

  invisible(filename)
}
