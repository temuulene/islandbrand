# Paths to files shipped inside the package. Each returns a path rather than
# the file's contents, so callers can pass it straight to Quarto, `officer`,
# `knitr::include_graphics()` or a copy.

.islh_path <- function(...) {
  path <- system.file(..., package = "islandbrand")
  if (!nzchar(path)) {
    .islh_abort(c(
      "Cannot find {.file {file.path(...)}} in the installed package.",
      i = "The installation may be incomplete; try reinstalling {.pkg islandbrand}."
    ))
  }
  path
}

#' Path to an Island Health logo file
#'
#' The brand system has two lockups in four colour variants. Full-colour is the
#' primary variant for both lockups, and dark blue is a second primary for the
#' horizontal lockup. Use white on dark backgrounds. **Black is for print
#' only** — do not use it on screen.
#'
#' Only the vector (`svg`) and raster (`png`) files ship with the package. The
#' Illustrator sources and print PDFs live in the `islh-brand-standard`
#' repository.
#'
#' @param lockup Logo arrangement: `"horizontal"` or `"stacked"`.
#' @param variant Colour variant: `"full-colour"`, `"dark-blue"`, `"white"` or
#'   `"black"`.
#' @param format File format: `"svg"` for anything scalable, `"png"` where a
#'   raster is required.
#'
#' @return A file path.
#' @export
#'
#' @examples
#' islh_logo()
#' islh_logo("stacked", "white", "png")
islh_logo <- function(
  lockup = c("horizontal", "stacked"),
  variant = c("full-colour", "dark-blue", "white", "black"),
  format = c("svg", "png")
) {
  lockup <- match.arg(lockup)
  variant <- match.arg(variant)
  format <- match.arg(format)

  .islh_path(
    "logos",
    paste0("islh-logo-", lockup, "-", variant, ".", format)
  )
}

#' Path to the Island Health `_brand.yml`
#'
#' The brand file Quarto and Shiny read for colours and typography. Use
#' [islh_use_brand()] to copy it into a project.
#'
#' @return A file path.
#' @export
#'
#' @examples
#' islh_brand_yml()
islh_brand_yml <- function() {
  .islh_path("brand", "_brand.yml")
}

#' Path to the Island Health Word reference document
#'
#' The `reference-doc` Quarto uses for `docx` output. It carries the letterhead
#' header, the page footer, BC Sans and the Island Health heading styles.
#'
#' @return A file path.
#' @export
#'
#' @examples
#' islh_reference_docx()
islh_reference_docx <- function() {
  .islh_path(
    "quarto",
    "_extensions",
    "islh",
    "islh-report",
    "islh-report-reference.docx"
  )
}

#' Example program counts
#'
#' A three-row table used in the package examples and in the report scaffold,
#' so a new report renders with something in it before you supply real data.
#'
#' @return A data frame with columns `program`, `encounters` and
#'   `median_wait_minutes`.
#' @export
#'
#' @examples
#' islh_example_data()
islh_example_data <- function() {
  utils::read.csv(
    .islh_path("extdata", "example-program-counts.csv"),
    stringsAsFactors = FALSE
  )
}

#' Example Island Health local health area map
#'
#' The boundaries of Island Health's 14 local health areas (LHAs) with their
#' 2025 population, bundled so map examples run without a network connection.
#' The columns match what `islandepi::islh_bc_geography()` and
#' `islandepi::islh_bc_population()` return once joined, so code written against
#' this object works on a current download.
#'
#' The boundaries are simplified for display: shared borders stay shared and
#' small islands are kept, but areas are approximate. Retrieve current
#' boundaries and denominators with `islandepi` for analysis.
#'
#' @return An `sf` object in BC Albers (EPSG:3005) with one row per LHA and
#'   columns `geography_code`, `geography_name`, `hsda`, `population`, `year`
#'   and `geometry`.
#'
#' @source Contains information licensed under the Open Government Licence -
#'   British Columbia. Boundaries: Local Health Area Boundaries, BC Data
#'   Catalogue,
#'   <https://catalogue.data.gov.bc.ca/dataset/afd021d9-7722-4410-b506-d394c66e74fc>.
#'   Population: BC Stats, BC Sub-Provincial Population Estimates and
#'   Projections,
#'   <https://catalogue.data.gov.bc.ca/dataset/86839277-986a-4a29-9f70-fa9b1166f6cb>.
#'   Built by `data-raw/build_example_lha.R`. The build record, with when the
#'   data was retrieved, the exact source addresses and the processing, is in
#'   `system.file("extdata", "islh-lha-provenance.csv", package =
#'   "islandbrand")`.
#'
#' @seealso [islh_areas()] for each LHA's brand colour.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   lha <- islh_example_lha()
#'   lha[c("geography_code", "geography_name", "population")]
#' }
islh_example_lha <- function() {
  .islh_require("sf", "the example local health area map")
  lha <- sf::st_read(
    .islh_path("extdata", "islh-lha.gpkg"),
    quiet = TRUE,
    stringsAsFactors = FALSE
  )
  sf::st_geometry(lha) <- "geometry"
  lha
}
