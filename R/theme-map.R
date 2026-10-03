#' Island Health map theme
#'
#' A map theme built on `ggplot2::theme_void()`, so no axis, tick, grid or
#' panel element has to be removed one at a time. It keeps the brand text
#' hierarchy from [theme_islh()] and nothing else. Pair it with
#' `ggplot2::geom_sf()`, [coord_islh_map()] and [scale_fill_islh_b()].
#'
#' Island Health runs northwest to southeast, which leaves an empty corner at
#' the top right of a full-Island map, over the mainland the map does not
#' draw. `legend = "inside"` puts the legend there and gives the map roughly a
#' fifth more room, which matters because `coord_sf()` fixes the aspect ratio
#' and a wider figure cannot stretch the map to fill it.
#'
#' @param base_size Base font size in points.
#' @param legend Legend placement: `"bottom"`, `"inside"` the panel, or
#'   `"none"`. The bar stays horizontal either way, so its break labels have
#'   room to sit side by side.
#' @param legend_inside Legend position when `legend = "inside"`, as fractions
#'   of the panel from the bottom left, placing the legend's bottom-left
#'   corner. The default sits in the empty top-right corner of a full-Island
#'   map. For a map of part of the Island, pick a corner that is clear.
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' ggplot2::ggplot() + theme_islh_map()
#' ggplot2::ggplot() + theme_islh_map(legend = "inside")
theme_islh_map <- function(
  base_size = 13,
  legend = c("bottom", "inside", "none"),
  legend_inside = c(0.62, 0.8)
) {
  legend <- match.arg(legend)
  base_size <- .islh_check_size(base_size)
  legend_inside <- .islh_check_position(legend_inside, "legend_inside")

  base <- ggplot2::theme_void(
    base_size = base_size,
    base_family = .islh_font()
  ) +
    .islh_text_theme() +
    ggplot2::theme(
      # `theme_void()` leaves the background transparent, which reads as a
      # broken figure in Word and PowerPoint.
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      # A map title aligns with the map, which a fixed aspect ratio keeps
      # narrower than the figure, so it runs a step smaller than a chart's.
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
      legend.key.size = grid::unit(0.9, "lines"),
      plot.margin = ggplot2::margin(6, 6, 6, 6)
    )

  base + .islh_map_legend(legend, legend_inside)
}

#' Legend placement for the map theme
#'
#' @param legend Legend placement.
#' @param inside Position within the panel.
#'
#' @return A ggplot2 theme object.
#'
#' @noRd
.islh_map_legend <- function(legend, inside) {
  switch(
    legend,
    bottom = ggplot2::theme(legend.position = "bottom"),
    none = ggplot2::theme(legend.position = "none"),
    # ggplot2 draws a legend vertically anywhere but the top or bottom edge,
    # which stacks a colour bar's break labels on top of each other.
    inside = ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = inside,
      legend.direction = "horizontal",
      legend.justification.inside = c(0, 0),
      legend.background = ggplot2::element_rect(
        fill = grDevices::adjustcolor("white", alpha.f = 0.7),
        colour = NA
      ),
      legend.margin = ggplot2::margin(4, 6, 4, 6)
    )
  )
}
