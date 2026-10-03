.islh_fallback_font <- "Noto Sans"

#' Resolve the Island Health font family
#'
#' @param refresh Refresh the system font cache and recheck installed fonts.
#' @param warn Warn when BC Sans and its fallback cannot be found.
#'
#' @return The first installed brand font, or an empty string so the graphics
#'   device can use its default font.
#'
#' @export
islh_font_family <- function(refresh = FALSE, warn = TRUE) {
  cached <- exists("font", envir = .islh_state, inherits = FALSE)
  if (!isTRUE(refresh) && cached) {
    return(.islh_state$font)
  }

  if (isTRUE(refresh)) {
    systemfonts::reset_font_cache()
  }
  installed <- unique(systemfonts::system_fonts()$family)
  hit <- intersect(c("BC Sans", .islh_fallback_font), installed)

  if (length(hit) == 0L) {
    if (isTRUE(warn)) {
      .islh_warn(c(
        # cli reads a leading dot inside braces as a style, and it evaluates
        # `{}` in the wrapper's frame, so build the name into the string.
        paste0(
          "Neither {.val BC Sans} nor {.val ",
          .islh_fallback_font,
          "} is installed."
        ),
        i = "Figures and tables will not match the Island Health standard."
      ))
    }
    .islh_state$font <- ""
    return("")
  }

  .islh_state$font <- hit[[1]]
  .islh_state$font
}

# On Windows, base graphics devices - the RStudio plot pane included - resolve
# family names through R's Windows font database, not through systemfonts. A
# font systemfonts can see is therefore still reported as "font family not
# found in Windows font database" when a plot is drawn on screen. Registering
# the family removes those warnings. ragg output and Quarto renders never
# needed it.
.islh_register_screen_font <- function(family) {
  if (!nzchar(family) || .Platform$OS.type != "windows") {
    return(invisible(FALSE))
  }
  if (identical(.islh_state$screen_font, family)) {
    return(invisible(TRUE))
  }

  registered <- tryCatch(
    {
      faces <- list(grDevices::windowsFont(family))
      names(faces) <- family
      do.call(grDevices::windowsFonts, faces)
      .islh_state$screen_font <- family
      TRUE
    },
    error = function(condition) FALSE
  )

  invisible(registered)
}

.islh_table_font <- function() {
  if (nzchar(.islh_font())) .islh_font() else "Arial"
}
