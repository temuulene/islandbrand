#' Print a short guide to the functions most people need
#'
#' @return The printed lines, invisibly.
#'
#' @examples
#' islh_help()
#'
#' @export
islh_help <- function() {
  lines <- c(
    paste0("Island Health theme ", islh_version()),
    "",
    "SETUP  once per document or session",
    "  islh_setup()                  apply the theme; detects HTML or Word",
    "  islh_reset()                  put the session back as it was",
    "  with_islh({ ... })            apply it around one block only",
    "  islh_check()                  list any packages you still need",
    "",
    "FIGURES  islh_setup() already applies the theme, so plot as usual",
    "  islh_epi_curve(data, date, count)  a routine surveillance curve",
    "  scale_fill_islh()             colours for categories",
    "  scale_colour_islh()           the same, for lines and points",
    "  scale_fill_islh_ordinal()     low to high within one colour",
    "  scale_fill_islh_signal()      red, orange, green for status",
    "  scale_y_islh_count()          count axis with thousands separators",
    "  theme_islh(base_size = 13)    the theme on its own, for one plot",
    "",
    "MAPS",
    "  theme_islh_map()              map theme with no chart furniture",
    '  theme_islh_map(legend = "inside")   legend in the empty corner',
    "  coord_islh_map()              BC Albers, no graticule",
    "  scale_fill_islh_b()           binned fill for a choropleth",
    "  scale_fill_islh_area()        a fixed colour for each of the 14 LHAs",
    "  islh_area_labels()            LHA codes on a map, without overlaps",
    '  islh_areas("lha")             LHA codes, names, HSDAs and colours',
    "  islh_example_lha()            the 14 LHAs with 2025 population, offline",
    "  islh_caption(source, extracted)     source, date, suppression rule",
    "",
    "COLOURS AND LOGOS",
    '  islh_brand("primary")         the main Island Health blue',
    '  islh_hex("blue", 40)          any step of any colour family',
    '  islh_logo("horizontal")       path to a logo file',
    "",
    "TABLES",
    "  islh_gt(data)                 HTML",
    "  islh_flextable(data)          Word",
    "  islh_gtsummary_gt(tbl)        a gtsummary table, for HTML",
    "  islh_gtsummary_flex(tbl)      a gtsummary table, for Word",
    "  both fill the text width; use width = 0.6 for a narrower table",
    "",
    "SAVING A FIGURE",
    '  islh_save_plot("figure.png")  standard report size',
    '  islh_save_plot("f.png", preset = "slide")',
    "",
    "STARTING A REPORT",
    "  islh_create_report(\"my-report\", format = \"docx\")",
    "  islh_install_deps(\"docx\")           install what the format needs",
    "",
    "KEEPING A REPORT UP TO DATE",
    "  islh_check_project()                what is out of date or edited",
    "  islh_update_project()               bring in the current files",
    "",
    "SEE IT WORK",
    "  islh_example_plot()                 a themed plot from simulated data"
  )

  cat(lines, sep = "\n")
  invisible(lines)
}

## Runnable example ----------------------------------------------------------

#' Create a runnable Island Health example plot
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontshow{assign("font", "", envir = getFromNamespace(".islh_state", "islandbrand"))}
#' islh_example_plot()
#'
#' @export
islh_example_plot <- function() {
  ggplot2::ggplot(
    datasets::mtcars,
    ggplot2::aes(x = wt, y = mpg, colour = factor(cyl))
  ) +
    ggplot2::geom_point(size = 3) +
    scale_colour_islh() +
    ggplot2::labs(
      title = "Fuel economy by vehicle weight",
      subtitle = "Example using the Island Health visual system",
      x = "Weight (1,000 lb)",
      y = "Miles per US gallon",
      colour = "Cylinders"
    ) +
    theme_islh(grid = "both")
}
