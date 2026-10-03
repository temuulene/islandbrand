#' Install the packages your output format needs
#'
#' Installs whatever [islh_check()] reports as missing or out of date.
#'
#' Island Health laptops block programs run from a user library, which breaks
#' installers that unpack with their own helper binary (`pak` is the usual
#' one), and they have no compiler, so a source build fails too. This uses base
#' R's [utils::install.packages()] with `type = "binary"` on Windows and macOS,
#' which is the route that works there. Linux has no CRAN binaries, so it falls
#' back to the platform default.
#'
#' @param format Output format to install for. `auto` detects Quarto HTML and
#'   DOCX renders; outside a render it selects plot-only setup. `"both"`
#'   installs the HTML and Word stacks together, for a project that renders to
#'   either.
#' @param quiet Suppress the status message.
#'
#' @return The packages installed, invisibly.
#' @export
#'
#' @examples
#' # Report what would be installed without installing it:
#' islh_check("html", quiet = TRUE)$missing
islh_install_deps <- function(
  format = c("auto", "html", "docx", "plots", "both"),
  quiet = FALSE
) {
  format <- match.arg(format)

  # A project that renders to both formats needs both table stacks. Checking
  # only one leaves the other render to fail at the table chunk.
  checks <- if (format == "both") {
    list(islh_check("html", quiet = TRUE), islh_check("docx", quiet = TRUE))
  } else {
    list(islh_check(format = format, quiet = TRUE))
  }
  check <- checks[[1]]
  wanted <- unique(unlist(lapply(checks, function(x) c(x$missing, x$outdated))))

  if (length(wanted) == 0L) {
    if (!isTRUE(quiet)) {
      .islh_inform(c(
        "v" = paste0(
          "Nothing to install; ",
          .islh_format_label(if (format == "both") "both" else check$format),
          " output is ready to go."
        )
      ))
    }
    return(invisible(character()))
  }

  if (!isTRUE(quiet)) {
    .islh_inform(c("i" = "Installing {.pkg {wanted}}."))
  }

  # Linux builds from source because CRAN publishes no Linux binaries; there,
  # asking for "binary" would fail outright.
  type <- if (
    .Platform$OS.type == "windows" || Sys.info()[["sysname"]] == "Darwin"
  ) {
    "binary"
  } else {
    getOption("pkgType")
  }

  .islh_install_packages(wanted, type = type)

  # A package already loaded keeps running its old version until R restarts.
  # islandbrand itself loads ggplot2, scales, systemfonts and cli, so an upgrade
  # of any of those is not in effect yet.
  loaded <- intersect(wanted, loadedNamespaces())

  if (!isTRUE(quiet)) {
    after <- if (format == "both") {
      html <- islh_check("html", quiet = TRUE)
      docx <- islh_check("docx", quiet = TRUE)
      if (html$ok) docx else html
    } else {
      islh_check(format = check$format, quiet = TRUE)
    }
    if (after$ok) {
      .islh_inform(c("v" = "Done. Run {.code islh_setup()} in your report."))
    } else {
      .islh_warn(c(
        "!" = "Some packages are still not ready.",
        .islh_problem_bullets(after)
      ))
    }
    if (length(loaded) > 0L) {
      .islh_inform(c(
        "!" = "Restart R before you render: {.pkg {loaded}} {?was/were}
               already loaded, so this session is still using the old
               version{?s}.",
        "i" = "In RStudio, use Session > Restart R."
      ))
    }
  }

  invisible(wanted)
}

# The one place packages are installed, so tests can replace it. Base R's
# installer is the only one that works on a locked-down laptop; see
# `islh_install_deps()`.
.islh_install_packages <- function(packages, type) {
  utils::install.packages(packages, type = type)
}
