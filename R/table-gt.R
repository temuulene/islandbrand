#' Apply Island Health styling to a gt table
#'
#' Styles a data frame or an existing gt table with the Island Health header
#' band, rules and type.
#'
#' @section Grouped rows and row labels:
#'
#' `groupname_col` gathers rows under a heading for each value of that column,
#' such as one heading per health service delivery area. `rowname_col` turns
#' a column into row labels at the left of the table, set apart by a thin
#' rule. Both are passed to [gt::gt()], so they apply when `data` is a data
#' frame; for a table built with `gt::gt()` already, set them there. The row
#' label column keeps its name as a header unless `stubhead` says otherwise.
#'
#' @param data A gt table or data frame.
#' @param groupname_col Column whose values become row group headings. Data
#'   frames only.
#' @param rowname_col Column whose values become row labels. Data frames only.
#' @param stubhead Header over the row labels. Defaults to the name of
#'   `rowname_col`; use `""` for none.
#' @param title,subtitle Optional table heading.
#' @param source_note Optional source note.
#' @param width Table width as a fraction of the available text width.
#' @param embed_fonts Embed the installed BC Sans faces as data URLs. The
#'   session default is set by `islh_setup()` and defaults to `TRUE` otherwise.
#'
#' @return A styled gt table.
#'
#' @examples
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   islh_gt(
#'     head(islh_example_data()),
#'     title = "Example encounters",
#'     source_note = "Source: simulated package data",
#'     embed_fonts = FALSE
#'   )
#'
#'   # One heading per HSDA, with the site as the row label.
#'   sites <- data.frame(
#'     hsda = c("South", "South", "Central", "North"),
#'     site = c("Site A", "Site B", "Site C", "Site D"),
#'     opened = c("2017-02-01", "2018-06-18", "2017-09-11", "2020-06-01")
#'   )
#'   islh_gt(
#'     sites,
#'     groupname_col = "hsda",
#'     rowname_col = "site",
#'     stubhead = "Site",
#'     title = "Site details",
#'     embed_fonts = FALSE
#'   )
#' }
#'
#' @export
islh_gt <- function(
  data,
  groupname_col = NULL,
  rowname_col = NULL,
  stubhead = NULL,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  width = 1,
  embed_fonts = getOption("islh.embed_fonts", TRUE)
) {
  .islh_require("gt", "Island Health HTML tables")
  width <- .islh_check_fraction(width, "width")
  embed_fonts <- .islh_check_flag(embed_fonts, "embed_fonts")

  if (inherits(data, "gt_tbl")) {
    if (!is.null(groupname_col) || !is.null(rowname_col)) {
      .islh_abort(c(
        "{.arg groupname_col} and {.arg rowname_col} need a data frame.",
        i = "Pass the data frame to {.fn islh_gt}, or set them in
             {.fn gt::gt} where the table is built."
      ))
    }
  } else {
    groupname_col <- .islh_check_column(groupname_col, data, "groupname_col")
    rowname_col <- .islh_check_column(rowname_col, data, "rowname_col")
    data <- gt::gt(
      data,
      groupname_col = groupname_col,
      rowname_col = rowname_col
    )
  }

  # gt leaves the row label column without a header, so a reader loses what
  # the labels are. Keep the column's name unless told otherwise.
  if (is.null(stubhead)) {
    stubhead <- rowname_col
  }
  if (!is.null(stubhead)) {
    if (!is.character(stubhead) || length(stubhead) != 1L || is.na(stubhead)) {
      .islh_abort("{.arg stubhead} must be one string.")
    }
    if (nzchar(stubhead)) {
      data <- gt::tab_stubhead(data, label = stubhead)
    }
  }

  if (!is.null(title) || !is.null(subtitle)) {
    data <- gt::tab_header(
      data,
      title = if (is.null(title)) "" else title,
      subtitle = subtitle
    )
  }

  if (!is.null(source_note)) {
    data <- gt::tab_source_note(data, source_note = source_note)
  }

  data <- gt::tab_options(
    data,
    table.font.names = unique(c(
      .islh_table_font(),
      .islh_fallback_font,
      "Arial",
      "sans-serif"
    )),
    # Without an explicit width, gt shrinks the table to its contents and
    # centres it, which reads as an afterthought beside full-width prose.
    table.width = gt::pct(width * 100),
    table.align = "left",
    table.margin.left = gt::px(0),
    table.margin.right = gt::px(0),
    table.font.size = gt::px(15),
    table.font.color = islh_brand("black"),
    table.font.color.light = islh_brand("white"),
    table.background.color = islh_brand("white"),
    heading.background.color = islh_brand("white"),
    heading.align = "left",
    heading.title.font.size = gt::px(22),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = gt::px(16),
    column_labels.background.color = islh_hex("blue", 20),
    column_labels.font.weight = "bold",
    column_labels.padding = gt::px(7),
    column_labels.border.top.style = "solid",
    column_labels.border.top.width = gt::px(1),
    column_labels.border.top.color = islh_hex("blue", 20),
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = gt::px(1),
    column_labels.border.bottom.color = islh_hex("blue", 20),
    table_body.hlines.style = "solid",
    table_body.hlines.width = gt::px(1),
    table_body.hlines.color = islh_hex("grey", 90),
    table_body.vlines.style = "none",
    table_body.border.bottom.style = "solid",
    table_body.border.bottom.width = gt::px(1),
    table_body.border.bottom.color = islh_hex("blue", 20),
    data_row.padding = gt::px(5),
    row_group.background.color = islh_hex("blue", 96),
    row_group.font.weight = "bold",
    row_group.border.top.color = islh_hex("grey", 90),
    row_group.border.bottom.color = islh_hex("grey", 90),
    stub.font.weight = "normal",
    stub.border.style = "solid",
    stub.border.width = gt::px(1),
    stub.border.color = islh_hex("grey", 90),
    footnotes.font.size = gt::px(13),
    source_notes.font.size = gt::px(13),
    table.border.top.style = "none",
    table.border.bottom.style = "none"
  )

  # gt gives the title cell a `gt_font_normal` class, which overrides the bold
  # weight set above. An inline style on the cell wins.
  if (.islh_gt_has_title(data)) {
    data <- gt::tab_style(
      data,
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_title("title")
    )
  }

  embed_in_table <- isTRUE(embed_fonts) &&
    !isTRUE(getOption("islh.document_webfont", FALSE))
  if (embed_in_table) {
    webfont_css <- .islh_bc_sans_webfont_css()
    if (nzchar(webfont_css)) {
      data <- gt::opt_css(data, css = webfont_css, add = FALSE)
    }
  }

  data
}

# Whether a gt table has a non-empty title, set here or by the caller.
.islh_gt_has_title <- function(data) {
  title <- tryCatch(data[["_heading"]][["title"]], error = function(e) NULL)
  length(title) == 1L && !is.na(title) && nzchar(as.character(title))
}
