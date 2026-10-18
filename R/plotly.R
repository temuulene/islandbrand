#' Make a ggplot interactive for an HTML report
#'
#' Adds Island Health styling and converts a ggplot to a Plotly widget with
#' hover labels, zooming and a clickable legend. Works with [islh_epi_curve()]
#' and ordinary ggplots. No server or online account is needed.
#'
#' @param plot A ggplot object. Existing layers, scales and data are kept.
#'   Its theme is kept; the Island Health theme is added only when no theme
#'   has been supplied.
#' @param tooltip Fields shown on hover, passed to [plotly::ggplotly()].
#'   Use `"none"` to hide hover labels, or a vector such as `c("x", "y")`.
#'   Epidemic curves use reporting dates and counts, with `"x"`, `"y"`,
#'   `"fill"`, `"facet"`, `"ymin"` and `"ymax"` selecting their fields.
#' @param width,height Widget dimensions in pixels. `NULL` uses the available
#'   width and the renderer's default height. Headings and notes sit outside
#'   these dimensions.
#' @param embed_fonts Embed installed BC Sans faces for readers without the
#'   font. Follows [islh_setup()] by default.
#' @param ... Additional named arguments to [plotly::ggplotly()].
#'
#' @return A `plotly` HTML widget, suitable for further [plotly::layout()] or
#'   [plotly::config()] calls and [htmlwidgets::saveWidget()].
#'
#' @section Reporting:
#' Print the widget as the last expression in a Quarto or R Markdown HTML
#' chunk. For one shareable Quarto file, set `embed-resources: true`.
#' Interactive output requires HTML; in Word or PDF chunks print the original
#' ggplot instead. Titles, subtitles and captions are plain text, placed
#' outside the plotting panel so they remain visible after zooming.
#' Epidemic-curve tooltips show each period's start date, rather than the
#' offset used to centre its bar. Case tiles show the period's group count,
#' and reference layers identify the mean and limits. Year separators have
#' no tooltip. Explicit axis and fill labels are also used in the tooltips.
#' Reference numbers display up to two decimal places without changing the
#' underlying values.
#'
#' Plotly converts common ggplot layers, but some extension geoms, map
#' coordinates and theme details are not supported. Check the rendered plot
#' before sharing. Use the original plot when conversion is unsuitable.
#'
#' Only pass data approved for inclusion in the report. Widgets embed data in
#' the HTML; hiding a hover field does not remove the underlying values.
#'
#' @examples
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point(colour = islh_brand("primary")) +
#'     ggplot2::labs(
#'       title = "Fuel economy", x = "Weight", y = "Miles per gallon"
#'     )
#'   islh_plotly(p, embed_fonts = FALSE)
#' }
#' @export
islh_plotly <- function(
  plot,
  tooltip = "all",
  width = NULL,
  height = NULL,
  embed_fonts = getOption("islh.embed_fonts", TRUE),
  ...
) {
  .islh_check_widget_output()
  .islh_require("plotly", "interactive Island Health plots")
  if (!inherits(plot, "ggplot")) {
    .islh_abort(
      paste0(
        "{.arg plot} must be a ggplot, such as the result of ",
        "{.fn islh_epi_curve}."
      )
    )
  }
  width <- .islh_widget_dimension(width, "width")
  height <- .islh_widget_dimension(height, "height")
  embed_fonts <- .islh_check_flag(embed_fonts, "embed_fonts")
  labels <- lapply(c("title", "subtitle", "caption"), function(name) {
    .islh_widget_label(plot$labels[[name]], paste0("plot label ", name))
  })
  if (!length(plot$theme)) {
    plot <- plot + theme_islh()
  }
  plot <- plot + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  hover <- .islh_prepare_epi_hover(plot, tooltip)
  args <- .islh_widget_args(
    list(p = hover$plot, tooltip = tooltip, width = width, height = height),
    list(...)
  )
  widget <- do.call(plotly::ggplotly, args)
  widget <- .islh_finish_epi_hover(widget, hover$state)
  widget <- plotly::config(
    widget,
    displaylogo = FALSE,
    scrollZoom = FALSE,
    responsive = TRUE
  )
  .islh_widget_labels(
    widget,
    labels[[1]],
    labels[[2]],
    labels[[3]],
    embed_fonts
  )
}
