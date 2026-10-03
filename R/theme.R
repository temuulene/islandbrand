## Plot theme -----------------------------------------------------------------

#' Brand text hierarchy shared by every Island Health theme
#'
#' Titles, subtitles, captions, legend text and strip labels read the same on
#' a bar chart and on a map. Keeping them in one place lets the map theme
#' start from `ggplot2::theme_void()`, which carries no chart furniture to
#' remove, and still match the rest of the package.
#'
#' @return A ggplot2 theme object.
#'
#' @noRd
.islh_text_theme <- function() {
  # Windows base devices, the RStudio plot pane included, resolve family names
  # through R's own font database rather than systemfonts. Registering here as
  # well as in `islh_setup()` keeps a plot drawn with a theme alone from
  # filling the console with "font family not found" warnings.
  .islh_register_screen_font(.islh_font())

  ggplot2::theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.3),
      face = "bold",
      colour = islh_hex("blue", 20),
      hjust = 0,
      margin = ggplot2::margin(b = 4)
    ),
    plot.subtitle = ggplot2::element_text(
      size = ggplot2::rel(1),
      colour = islh_hex("grey", 25),
      hjust = 0,
      margin = ggplot2::margin(b = 10)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.85),
      hjust = 0,
      colour = islh_hex("grey", 30),
      margin = ggplot2::margin(t = 10)
    ),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(
      size = ggplot2::rel(0.92),
      colour = islh_hex("grey", 15)
    ),
    legend.title = ggplot2::element_text(
      size = ggplot2::rel(0.92),
      colour = islh_hex("grey", 15)
    ),
    strip.text = ggplot2::element_text(
      size = ggplot2::rel(1),
      face = "bold",
      colour = islh_hex("blue", 20),
      hjust = 0
    )
  )
}

#' Island Health ggplot theme
#'
#' @param base_size Base font size in points.
#' @param grid Major gridlines to display: y, x, both, or none.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' \dontshow{assign("font", "", envir = getFromNamespace(".islh_state", "islandbrand"))}
#' ggplot2::ggplot(
#'   datasets::mtcars,
#'   ggplot2::aes(wt, mpg)
#' ) +
#'   ggplot2::geom_point(colour = islh_brand("primary"), size = 3) +
#'   ggplot2::labs(
#'     title = "Fuel economy by vehicle weight",
#'     x = "Weight (1,000 lb)",
#'     y = "Miles per US gallon"
#'   ) +
#'   theme_islh(grid = "both")
#'
#' @export
theme_islh <- function(base_size = 13, grid = c("y", "x", "both", "none")) {
  grid <- match.arg(grid)
  base_size <- .islh_check_size(base_size)

  base <- ggplot2::theme_minimal(
    base_size = base_size,
    base_family = .islh_font()
  ) +
    .islh_text_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(1),
        colour = islh_hex("grey", 15)
      ),
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.92),
        colour = islh_hex("grey", 15)
      ),
      panel.grid.major = ggplot2::element_line(
        colour = islh_hex("grey", 93),
        linewidth = 0.4
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key.size = grid::unit(0.9, "lines"),
      plot.margin = ggplot2::margin(6, 10, 6, 6)
    )

  # Every axis that shows a line and ticks draws them the same way.
  axis_rule <- ggplot2::element_line(
    colour = islh_hex("grey", 50),
    linewidth = 0.4
  )
  x_axis <- ggplot2::theme(axis.line.x = axis_rule, axis.ticks.x = axis_rule)
  y_axis <- ggplot2::theme(axis.line.y = axis_rule, axis.ticks.y = axis_rule)

  base +
    switch(
      grid,
      y = ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
        x_axis,
      x = ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
        y_axis,
      both = x_axis + y_axis,
      none = ggplot2::theme(panel.grid.major = ggplot2::element_blank()) +
        x_axis +
        y_axis
    )
}

## Plot helpers ---------------------------------------------------------------

#' Apply Island Health plot defaults for the current R session
#'
#' @param base_size Base font size in points.
#' @param grid Major gridlines to display.
#' @param set_knitr Use `ragg_png` for subsequent knitr chunks when available.
#'
#' @return Previous theme and options, invisibly.
#'
#' @noRd
.islh_use_theme <- function(
  base_size = 13,
  grid = c("y", "x", "both", "none"),
  set_knitr = TRUE
) {
  grid <- match.arg(grid)
  base_size <- .islh_check_size(base_size)
  set_knitr <- .islh_check_flag(set_knitr, "set_knitr")

  .islh_register_screen_font(.islh_font())
  old_theme <- ggplot2::theme_set(
    theme_islh(base_size = base_size, grid = grid)
  )
  old_options <- options(
    ggplot2.discrete.colour = scale_colour_islh,
    ggplot2.discrete.fill = scale_fill_islh
  )

  ggplot2::update_geom_defaults("bar", list(fill = islh_brand("primary")))
  ggplot2::update_geom_defaults("col", list(fill = islh_brand("primary")))
  ggplot2::update_geom_defaults("line", list(colour = islh_brand("primary")))
  ggplot2::update_geom_defaults("point", list(colour = islh_brand("primary")))
  ggplot2::update_geom_defaults("smooth", list(colour = islh_brand("primary")))
  ggplot2::update_geom_defaults("area", list(fill = islh_brand("primary")))

  if (isTRUE(set_knitr) && requireNamespace("knitr", quietly = TRUE)) {
    if (requireNamespace("ragg", quietly = TRUE)) {
      knitr::opts_chunk$set(dev = "ragg_png")
    } else {
      .islh_warn(c(
        "Package {.pkg ragg} is not installed.",
        i = "Quarto is using its existing graphics device."
      ))
    }
  }

  invisible(list(theme = old_theme, options = old_options))
}

#' Count axis with Island Health defaults
#'
#' Breaks fall on whole numbers only. Counts cannot be fractional, and the
#' labels round to whole numbers, so a break at 2.5 would print as "2" beside
#' the wrong gridline.
#'
#' @param ... Additional arguments passed to `ggplot2::scale_y_continuous()`.
#' @param breaks Break positions or a function that returns them. The default
#'   keeps breaks on whole numbers.
#' @param labels Label function.
#' @param expand Scale expansion. The lower limit stays on the baseline.
#'
#' @return A ggplot2 continuous position scale.
#'
#' @examples
#' \dontshow{assign("font", "", envir = getFromNamespace(".islh_state", "islandbrand"))}
#' counts <- islh_example_data()
#'
#' ggplot2::ggplot(counts, ggplot2::aes(program, encounters)) +
#'   ggplot2::geom_col(fill = islh_brand("primary")) +
#'   scale_y_islh_count() +
#'   theme_islh()
#'
#' @export
scale_y_islh_count <- function(
  ...,
  breaks = .islh_count_breaks(),
  labels = scales::label_comma(accuracy = 1),
  expand = ggplot2::expansion(mult = c(0, 0.05))
) {
  ggplot2::scale_y_continuous(
    ...,
    breaks = breaks,
    labels = labels,
    expand = expand
  )
}

# Whole-number breaks for a count axis. The steps leave out 2.5, which the
# default algorithm prefers and which cannot label a count. A range too short
# for two whole-number steps falls back to every whole number in it.
.islh_count_breaks <- function(n = 5) {
  extended <- scales::breaks_extended(n = n, Q = c(1, 5, 2, 4, 3))
  function(limits) {
    breaks <- extended(limits)
    whole <- breaks[abs(breaks - round(breaks)) < 1e-8]
    if (length(whole) >= 2L) {
      return(whole)
    }
    low <- ceiling(min(limits))
    high <- floor(max(limits))
    if (low > high) whole else seq(low, high)
  }
}
