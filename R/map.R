#' BC Albers coordinates for Island Health maps
#'
#' Sets the coordinate reference system to BC Albers (EPSG:3005), the
#' provincial standard that BC Data Catalogue and BC Stats layers already use.
#' Latitude and longitude (EPSG:4326) stretches Vancouver Island sideways and
#' distorts area, so a map drawn in it misrepresents how large each local
#' health area is.
#'
#' It also drops the graticule that `ggplot2::coord_sf()` draws by default and
#' turns off the expansion around the data, both of which are chart furniture
#' on a projected map.
#'
#' @param crs Coordinate reference system. BC Albers unless you have a reason.
#' @param datum Graticule datum. `NA` draws no graticule.
#' @param expand Expand the panel beyond the data.
#' @param ... Additional arguments passed to `ggplot2::coord_sf()`, such as
#'   `xlim` and `ylim` for an inset panel.
#'
#' @return A ggplot2 coordinate system.
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot(boundaries) +
#'   ggplot2::geom_sf() +
#'   coord_islh_map()
#' }
coord_islh_map <- function(crs = 3005, datum = NA, expand = FALSE, ...) {
  .islh_require("sf", "drawing maps")

  ggplot2::coord_sf(crs = crs, datum = datum, expand = expand, ...)
}

#' Build a map or figure caption from its required parts
#'
#' A caption on a health figure has a job: it says where the numbers came
#' from, when they were pulled, what they are rates of, and which cells were
#' suppressed. Assembling it from named parts means no part is quietly left
#' out of a report.
#'
#' Any argument left `NULL` is skipped, so a count map can omit
#' `standard_pop`. Give `governance` a data governance statement where the
#' geography covers First Nations communities.
#'
#' ggplot2 does not wrap a caption, so a full one runs off the side of a
#' figure. The text is wrapped at `width` characters instead; 100 suits the
#' 6.5-inch report preset in [islh_save_plot()].
#'
#' @param source Data source, for example `"BC Data Catalogue"`.
#' @param extracted Extraction date, as a string or a `Date`.
#' @param boundary Geography and boundary vintage, for example
#'   `"Local health areas, 2024 boundaries"`.
#' @param standard_pop Standard population behind an age-standardized rate.
#' @param suppression Suppression rule applied before mapping.
#' @param governance Data governance statement, where one applies.
#' @param width Characters per line before wrapping. `Inf` leaves the caption
#'   on one line. A map draws narrower than its figure, because
#'   [coord_islh_map()] fixes the aspect ratio, so use about 60 for a
#'   full-Island map.
#'
#' @return A single string for `ggplot2::labs(caption = )`.
#' @export
#'
#' @examples
#' islh_caption(
#'   source = "BC Data Catalogue",
#'   extracted = "2026-03-31",
#'   boundary = "Local health areas, 2024 boundaries",
#'   standard_pop = "2011 Canadian standard population",
#'   suppression = "Counts under 5 suppressed."
#' )
islh_caption <- function(
  source,
  extracted,
  boundary = NULL,
  standard_pop = NULL,
  suppression = NULL,
  governance = NULL,
  width = 80
) {
  if (!.islh_is_string(source) || !.islh_is_string(extracted)) {
    .islh_abort("{.arg source} and {.arg extracted} must each be one string.")
  }

  parts <- c(
    paste0("Source: ", source, "."),
    paste0("Extracted ", format(extracted), "."),
    if (!is.null(boundary)) paste0(boundary, "."),
    if (!is.null(standard_pop)) paste0("Standardized to ", standard_pop, "."),
    if (!is.null(suppression)) .islh_sentence(suppression),
    if (!is.null(governance)) .islh_sentence(governance)
  )

  paste(strwrap(paste(parts, collapse = " "), width = width), collapse = "\n")
}

#' Is this one non-missing string?
#'
#' @param x Value to test.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
.islh_is_string <- function(x) {
  (is.character(x) || inherits(x, "Date")) && length(x) == 1L && !is.na(x)
}

#' Close a caption fragment with a full stop unless it has one
#'
#' @param x Caption fragment.
#'
#' @return The fragment, ending in punctuation.
#'
#' @noRd
.islh_sentence <- function(x) {
  x <- trimws(as.character(x))
  if (grepl("[.!?]$", x)) x else paste0(x, ".")
}
