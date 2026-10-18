# Shared styling and validation for report widgets.
.islh_check_widget_output <- function(call = rlang::caller_env()) {
  if (
    isTRUE(getOption("knitr.in.progress", FALSE)) &&
      requireNamespace("knitr", quietly = TRUE) &&
      !knitr::is_html_output()
  ) {
    .islh_abort(
      c(
        "Interactive output needs an HTML report.",
        i = paste0(
          "For Word or PDF, print the original ggplot or use ",
          "{.fn islh_flextable}."
        ),
        i = paste0(
          "In a report with multiple formats, use ",
          "{.fn knitr::is_html_output} to choose."
        )
      ),
      call = call
    )
  }
}

.islh_widget_label <- function(x, arg, call = rlang::caller_env()) {
  if (
    !is.null(x) &&
      (!is.character(x) || length(x) != 1L || is.na(x))
  ) {
    .islh_abort("{.arg {arg}} must be one string or NULL.", call = call)
  }
  if (is.null(x)) NULL else as.character(x)
}

.islh_widget_dimension <- function(x, arg, call = rlang::caller_env()) {
  if (
    !is.null(x) &&
      (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0)
  ) {
    .islh_abort(
      "{.arg {arg}} must be one positive number of pixels or NULL.",
      call = call
    )
  }
  x
}

.islh_widget_font <- function() {
  paste(
    c(
      sprintf(
        '"%s"',
        unique(c(
          .islh_table_font(),
          .islh_fallback_font,
          "Arial"
        ))
      ),
      "sans-serif"
    ),
    collapse = ", "
  )
}

.islh_widget_labels <- function(widget, title, subtitle, note, embed_fonts) {
  .islh_require("htmltools", "interactive report styling")
  .islh_require("htmlwidgets", "interactive report styling")
  font <- paste0("font-family:", .islh_widget_font(), ";")
  if (!is.null(title) || !is.null(subtitle)) {
    heading <- htmltools::tags$div(
      class = "islh-widget-heading",
      style = paste0(font, "text-align:left;margin-bottom:10px;"),
      if (!is.null(title)) {
        htmltools::tags$div(
          role = "heading",
          `aria-level` = "3",
          style = paste0(
            "font-size:22px;font-weight:bold;color:",
            islh_hex("blue", 20),
            ";"
          ),
          title
        )
      },
      if (!is.null(subtitle)) {
        htmltools::tags$div(
          style = paste0("font-size:16px;color:", islh_hex("grey", 25), ";"),
          subtitle
        )
      }
    )
    widget <- htmlwidgets::prependContent(widget, heading)
  }
  if (!is.null(note)) {
    widget <- htmlwidgets::appendContent(
      widget,
      htmltools::tags$div(
        class = "islh-widget-note",
        style = paste0(
          font,
          "font-size:13px;color:",
          islh_hex("grey", 30),
          ";margin-top:10px;white-space:pre-line;"
        ),
        note
      )
    )
  }
  if (embed_fonts && !.islh_document_has_webfont()) {
    css <- .islh_bc_sans_webfont_css()
    if (nzchar(css)) {
      directory <- file.path(
        tempdir(),
        paste0("islh-widget-font-", islh_version())
      )
      dir.create(directory, recursive = TRUE, showWarnings = FALSE)
      writeLines(css, file.path(directory, "bc-sans.css"), useBytes = TRUE)
      widget$dependencies <- c(
        widget$dependencies,
        list(htmltools::htmlDependency(
          name = "islh-widget-font",
          version = as.character(islh_version()),
          src = c(file = directory),
          stylesheet = "bc-sans.css",
          all_files = FALSE
        ))
      )
    }
  }
  widget
}

.islh_widget_args <- function(defaults, extra, call = rlang::caller_env()) {
  if (length(extra) > 0L) {
    if (
      is.null(names(extra)) ||
        anyNA(names(extra)) ||
        any(!nzchar(names(extra))) ||
        anyDuplicated(names(extra))
    ) {
      .islh_abort(
        "Arguments in {.arg ...} must have unique names.",
        call = call
      )
    }
    if ("data" %in% names(extra) || "p" %in% names(extra)) {
      .islh_abort("Supply the data or plot as the first argument.", call = call)
    }
    defaults[names(extra)] <- extra
  }
  defaults
}
