#' Create an interactive Island Health table
#'
#' Turns a data frame into a branded table with search, sorting and pagination.
#' Tables run locally in the reader's browser and need no server.
#'
#' @param data A data frame containing only the columns intended for readers.
#' @param title,subtitle Optional plain-text table heading.
#' @param source_note Optional plain-text source note below the table.
#' @param searchable Show a search box for all columns.
#' @param filterable Show individual column filters.
#' @param page_size Number of rows per page, a positive whole number.
#' @param embed_fonts Embed installed BC Sans faces for readers without the
#'   font. Follows [islh_setup()] by default.
#' @param ... Additional named [reactable::reactable()] arguments. For example,
#'   use `columns` with [reactable::colDef()] for labels and number formatting,
#'   `groupBy` for row groups, or `defaultSorted` for initial sorting. These
#'   override the corresponding defaults, including `theme` and `defaultColDef`.
#'
#' @return A `reactable` HTML widget that can be printed in a Quarto or
#'   R Markdown HTML chunk or saved with [htmlwidgets::saveWidget()].
#'
#' @section Reporting:
#' Print the table as the last expression in an HTML chunk. Quarto's
#' `embed-resources: true` produces one shareable file. For Word output use
#' [islh_flextable()] and for static HTML tables use [islh_gt()].
#'
#' Text is escaped by default. Only enable `html = TRUE` in a column definition
#' for trusted markup. All supplied data, including hidden columns and rows
#' on other pages, is embedded in the HTML. Select and suppress values before
#' calling this function; hiding them in the table is not disclosure control.
#'
#' @examples
#' if (requireNamespace("reactable", quietly = TRUE)) {
#'   islh_reactable(
#'     head(islh_example_data()),
#'     title = "Example encounters",
#'     source_note = "Source: simulated package data",
#'     embed_fonts = FALSE
#'   )
#' }
#' @export
islh_reactable <- function(
  data,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  searchable = TRUE,
  filterable = FALSE,
  page_size = 10,
  embed_fonts = getOption("islh.embed_fonts", TRUE),
  ...
) {
  .islh_check_widget_output()
  .islh_require("reactable", "interactive Island Health tables")
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  title <- .islh_widget_label(title, "title")
  subtitle <- .islh_widget_label(subtitle, "subtitle")
  source_note <- .islh_widget_label(source_note, "source_note")
  searchable <- .islh_check_flag(searchable, "searchable")
  filterable <- .islh_check_flag(filterable, "filterable")
  page_size <- .islh_check_count(page_size, "page_size")
  embed_fonts <- .islh_check_flag(embed_fonts, "embed_fonts")

  args <- .islh_widget_args(
    list(
      data = data,
      searchable = searchable,
      filterable = filterable,
      defaultPageSize = page_size,
      highlight = TRUE,
      borderless = TRUE,
      defaultColDef = reactable::colDef(na = "n/a"),
      theme = reactable::reactableTheme(
        color = islh_brand("black"),
        backgroundColor = islh_brand("white"),
        borderColor = islh_hex("grey", 90),
        highlightColor = islh_hex("blue", 96),
        style = list(fontFamily = .islh_widget_font(), fontSize = "15px"),
        headerStyle = list(
          background = islh_hex("blue", 20),
          color = islh_brand("white"),
          fontWeight = "bold"
        ),
        cellStyle = list(padding = "7px 10px"),
        searchInputStyle = list(borderColor = islh_brand("secondary")),
        pageButtonStyle = list(color = islh_hex("blue", 20))
      )
    ),
    list(...)
  )
  widget <- do.call(reactable::reactable, args)
  .islh_widget_labels(widget, title, subtitle, source_note, embed_fonts)
}
