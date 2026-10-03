# Which output format are we configuring for?
#
# Under Quarto and knitr this can be detected. Outside a render there is no
# document to inspect, so plot-only setup is the honest answer.
.islh_resolve_format <- function(format = c("auto", "html", "docx", "plots")) {
  format <- match.arg(format)
  if (format != "auto") {
    return(format)
  }

  if (!requireNamespace("knitr", quietly = TRUE)) {
    return("plots")
  }

  if (isTRUE(knitr::is_html_output())) {
    return("html")
  }

  if (identical(knitr::pandoc_to(), "docx")) {
    return("docx")
  }

  "plots"
}

# How a format is named in messages: the words staff use, not the codes.
.islh_format_label <- function(format) {
  labels <- c(
    html = "HTML",
    docx = "Word",
    plots = "plots",
    both = "HTML and Word"
  )
  unname(labels[format])
}

# Minimum versions the package actually relies on. Each was found by running
# the code against an older release, not by caution:
#
# * ggplot2 3.5.0 made `discrete_scale()`'s `scale_name` argument optional. The
#   discrete scales omit it, so an older ggplot2 fails inside ggplot2 itself
#   with `argument "scale_name" is missing`.
# * flextable 0.9.10 added the `repeat_headers` Word option that
#   `islh_flextable()` sets, so an older flextable errors with
#   `unused argument (repeat_headers = TRUE)`.
# * gtsummary 2.0.0 added the `label_style_*()` functions the table theme
#   uses. gtsummary is optional, so an older one is skipped, not required.
.islh_min_versions <- c(
  ggplot2 = "3.5.0",
  flextable = "0.9.10",
  gtsummary = "2.0.0"
)

# Which of `packages` are installed but older than the package needs?
.islh_outdated <- function(packages) {
  pinned <- intersect(packages, names(.islh_min_versions))
  installed <- pinned[
    vapply(pinned, requireNamespace, logical(1), quietly = TRUE)
  ]
  too_old <- vapply(
    installed,
    function(p) utils::packageVersion(p) < .islh_min_versions[[p]],
    logical(1)
  )
  installed[too_old]
}

#' Check dependencies for an Island Health output format
#'
#' Reports which packages the requested output format needs and which are
#' missing, without installing anything. The required set is deliberately
#' format-specific: HTML reports need `gt` but not the Word packages, and Word
#' reports need `flextable` and `officer` but not `gt`.
#'
#' @param format Output format. `auto` detects Quarto HTML and DOCX renders;
#'   outside a render it selects plot-only setup.
#' @param tables Configure table packages for the selected format.
#' @param embed_fonts Embed BC Sans in HTML tables.
#' @param quiet Suppress the human-readable status message.
#'
#' @return An object of class `islh_dependency_check`, invisibly.
#' @export
#'
#' @examples
#' islh_check(format = "plots", quiet = TRUE)
islh_check <- function(
  format = c("auto", "html", "docx", "plots"),
  tables = TRUE,
  embed_fonts = TRUE,
  quiet = FALSE
) {
  format <- .islh_resolve_format(format)
  tables <- .islh_check_flag(tables, "tables") && format != "plots"
  embed_fonts <- .islh_check_flag(embed_fonts, "embed_fonts") &&
    format == "html" &&
    tables
  quiet <- .islh_check_flag(quiet, "quiet")

  required <- c("ggplot2", "cli", "scales", "systemfonts")
  if (tables && format == "html") {
    required <- c(required, "gt")
    if (embed_fonts) {
      required <- c(required, "base64enc")
    }
  }
  if (tables && format == "docx") {
    required <- c(required, "flextable", "officer")
  }
  required <- unique(required)
  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]

  outdated <- .islh_outdated(required)

  result <- list(
    ok = length(missing) == 0L && length(outdated) == 0L,
    format = format,
    tables = tables,
    embed_fonts = embed_fonts,
    required = required,
    missing = missing,
    outdated = outdated,
    install_command = .islh_install_command(
      if (length(missing) > 0L || length(outdated) > 0L) {
        c(missing, outdated)
      } else {
        required
      }
    )
  )
  class(result) <- "islh_dependency_check"

  if (!isTRUE(quiet)) {
    print(result)
  }

  invisible(result)
}

# Bullets describing what is wrong with a dependency check. Shared by the
# print method and by `islh_setup()`'s error so the two cannot disagree.
.islh_problem_bullets <- function(x) {
  bullets <- character()
  if (length(x$missing) > 0L) {
    bullets <- c(
      bullets,
      "x" = paste0(
        "Not installed: ",
        paste(x$missing, collapse = ", "),
        "."
      )
    )
  }
  for (pkg in x$outdated) {
    bullets <- c(
      bullets,
      "x" = paste0(
        pkg,
        " ",
        utils::packageVersion(pkg),
        " is too old; ",
        .islh_min_versions[[pkg]],
        " or newer is required."
      )
    )
  }
  c(bullets, "i" = paste0("Install with {.code ", x$install_command, "}."))
}

#' @param x An `islh_dependency_check` object.
#' @param ... Ignored.
#' @rdname islh_check
#' @export
print.islh_dependency_check <- function(x, ...) {
  if (x$ok) {
    .islh_inform(c(
      "v" = paste0(
        "Island Health theme dependencies are ready for ",
        .islh_format_label(x$format),
        "."
      )
    ))
    return(invisible(x))
  }

  .islh_inform(.islh_problem_bullets(x))
  invisible(x)
}
