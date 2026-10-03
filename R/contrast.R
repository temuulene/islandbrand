#' Perceptual lightness of a colour
#'
#' CIE L*, from 0 for black to 100 for white. It is what a greyscale print
#' leaves of a colour, so it is the measure that says whether a sequential
#' ramp still reads without hue, and whether a missing-data fill can be told
#' apart from the bins around it.
#'
#' @param colour A character vector of colours.
#'
#' @return A numeric vector of L* values.
#'
#' @noRd
.islh_lightness <- function(colour) {
  rgb <- t(grDevices::col2rgb(colour)) / 255
  grDevices::convertColor(rgb, from = "sRGB", to = "Lab")[, "L"]
}

#' Calculate a WCAG contrast ratio
#'
#' @param foreground Foreground colour as a name or hexadecimal value.
#' @param background Background colour as a name or hexadecimal value.
#'
#' @return A numeric vector of contrast ratios from 1 to 21.
#'
#' @noRd
.islh_contrast_ratio <- function(foreground, background) {
  lengths <- c(length(foreground), length(background))
  output_length <- max(lengths)

  if (any(!lengths %in% c(1L, output_length))) {
    .islh_abort(
      "{.arg foreground} and {.arg background} must have compatible lengths."
    )
  }

  foreground <- rep(foreground, length.out = output_length)
  background <- rep(background, length.out = output_length)

  relative_luminance <- function(colour) {
    rgb <- tryCatch(
      grDevices::col2rgb(colour) / 255,
      error = function(cnd) {
        .islh_abort(
          "Could not interpret one or more colours.",
          parent = cnd
        )
      }
    )
    rgb <- ifelse(
      rgb <= 0.04045,
      rgb / 12.92,
      ((rgb + 0.055) / 1.055)^2.4
    )
    drop(c(0.2126, 0.7152, 0.0722) %*% rgb)
  }

  foreground_luminance <- relative_luminance(foreground)
  background_luminance <- relative_luminance(background)

  (pmax(foreground_luminance, background_luminance) + 0.05) /
    (pmin(foreground_luminance, background_luminance) + 0.05)
}
