# Quarto reads extensions and `_brand.yml` from the project directory, not from
# an R library, so these helpers copy the shipped files in.
#
# Every write is checked. On a managed laptop these run against network shares,
# synced folders and directories the user may not own, where a copy can fail
# for reasons R reports only through the return value. Reporting success for a
# file that was never written is the worst outcome, because the render then
# fails somewhere else entirely.

.islh_mkdir <- function(path, call = rlang::caller_env()) {
  if (dir.exists(path)) {
    return(invisible(path))
  }
  ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!ok || !dir.exists(path)) {
    .islh_abort(
      c(
        "Could not create {.file {path}}.",
        i = "Check that you have permission to write there and that the path is
           not too long."
      ),
      call = call
    )
  }
  invisible(path)
}

.islh_copy <- function(from, to, call = rlang::caller_env()) {
  .islh_mkdir(dirname(to), call = call)
  ok <- file.copy(from, to, overwrite = TRUE)
  if (!ok || !file.exists(to)) {
    .islh_abort(
      c(
        "Could not write {.file {to}}.",
        i = "The file may be open in another program, or the folder may be
           read-only."
      ),
      call = call
    )
  }
  invisible(to)
}

.islh_write_lines <- function(lines, path, call = rlang::caller_env()) {
  .islh_mkdir(dirname(path), call = call)
  result <- tryCatch(
    {
      writeLines(lines, path)
      TRUE
    },
    error = function(condition) conditionMessage(condition)
  )
  if (!isTRUE(result) || !file.exists(path)) {
    .islh_abort(
      c(
        "Could not write {.file {path}}.",
        x = if (isTRUE(result)) "The file is missing after writing." else result
      ),
      call = call
    )
  }
  invisible(path)
}

# Copies a directory tree, reporting each file and refusing to clobber unless
# asked. Returns the paths written, relative to `dir`.
.islh_copy_into <- function(source, target, dir, overwrite) {
  files <- list.files(source, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  written <- character()
  skipped <- character()

  for (file in files) {
    destination <- file.path(target, file)
    if (file.exists(destination) && !isTRUE(overwrite)) {
      skipped <- c(skipped, file)
      next
    }
    .islh_copy(file.path(source, file), destination)
    written <- c(written, destination)
  }

  list(
    written = .islh_relative(written, dir),
    skipped = skipped
  )
}

.islh_relative <- function(paths, dir) {
  if (length(paths) == 0L) {
    return(character())
  }
  prefix <- paste0(normalizePath(dir, mustWork = FALSE), .Platform$file.sep)
  sub(prefix, "", normalizePath(paths, mustWork = FALSE), fixed = TRUE)
}

.islh_report_copy <- function(result, what, overwrite) {
  if (length(result$written) > 0L) {
    .islh_inform(c("v" = paste0("Wrote ", what, ":")))
    .islh_inform(stats::setNames(
      paste0("{.file ", result$written, "}"),
      rep("*", length(result$written))
    ))
  }
  skipped <- length(result$skipped)
  if (skipped > 0L && !isTRUE(overwrite)) {
    .islh_inform(c(
      "i" = paste0(
        "{skipped} file{?s} already existed and {?was/were} left alone.",
        " Pass {.code overwrite = TRUE} to replace {?it/them}."
      )
    ))
  }
  invisible(result$written)
}

#' Add the Island Health Quarto format to a project
#'
#' Copies the `islh-report` Quarto extension into `dir/_extensions`. Quarto
#' only looks for extensions inside the project directory, so this has to be a
#' copy rather than a reference into the R library. Re-run it after upgrading
#' `islandbrand` to pick up format changes.
#'
#' Afterwards, set the format in the document's front matter or `_quarto.yml`:
#'
#' ```yaml
#' format: islh-report-html    # or islh-report-docx
#' ```
#'
#' @param dir Project directory. Defaults to the working directory.
#' @param overwrite Replace files that are already there.
#'
#' @return The paths written, relative to `dir`, invisibly.
#' @export
#'
#' @examples
#' project <- file.path(tempdir(), "example-report")
#' dir.create(project, showWarnings = FALSE)
#' islh_use_quarto(project)
islh_use_quarto <- function(dir = ".", overwrite = FALSE) {
  .islh_check_dir(dir)
  source <- .islh_path("quarto", "_extensions")
  result <- .islh_copy_into(
    source,
    file.path(dir, "_extensions"),
    dir,
    overwrite
  )
  .islh_report_copy(result, "the Island Health Quarto format", overwrite)
}

#' Add the Island Health brand file to a project
#'
#' Copies `_brand.yml` into `dir`, together with the logo files it references.
#' Quarto 1.6 and later, and Shiny via `bslib`, read it for the Island Health
#' palette and typography.
#'
#' The logos travel with it by default. `_brand.yml`'s logo paths are relative
#' to the file itself, so a `_brand.yml` copied on its own would name images
#' that are not there.
#'
#' @param dir Project directory. Defaults to the working directory.
#' @param logos Copy the logo files `_brand.yml` references into `dir/logos`.
#' @param overwrite Replace files that are already there.
#'
#' @return The paths written, relative to `dir`, invisibly.
#' @export
#'
#' @examples
#' project <- file.path(tempdir(), "example-brand")
#' dir.create(project, showWarnings = FALSE)
#' islh_use_brand(project)
islh_use_brand <- function(dir = ".", logos = TRUE, overwrite = FALSE) {
  .islh_check_dir(dir)
  written <- character()
  skipped <- character()

  destination <- file.path(dir, "_brand.yml")
  if (file.exists(destination) && !isTRUE(overwrite)) {
    skipped <- "_brand.yml"
  } else {
    .islh_copy(islh_brand_yml(), destination)
    written <- destination
  }

  if (isTRUE(logos)) {
    for (name in .islh_brand_logo_files()) {
      target <- file.path(dir, "logos", name)
      if (file.exists(target) && !isTRUE(overwrite)) {
        skipped <- c(skipped, file.path("logos", name))
        next
      }
      .islh_copy(.islh_path("logos", name), target)
      written <- c(written, target)
    }
  }

  .islh_report_copy(
    list(written = .islh_relative(written, dir), skipped = skipped),
    "the Island Health brand file",
    overwrite
  )
}

# The logo files `_brand.yml` names, read out of the file itself so the two
# cannot drift apart. Falls back to the shipped list when `yaml` is absent.
.islh_brand_logo_files <- function() {
  fallback <- paste0(
    "islh-logo-",
    c(
      "stacked-full-colour",
      "stacked-dark-blue",
      "stacked-white",
      "horizontal-full-colour",
      "horizontal-dark-blue",
      "horizontal-white"
    ),
    ".svg"
  )

  if (!requireNamespace("yaml", quietly = TRUE)) {
    return(fallback)
  }

  images <- tryCatch(
    yaml::read_yaml(islh_brand_yml())$logo$images,
    error = function(condition) NULL
  )
  if (is.null(images)) {
    return(fallback)
  }

  basename(unlist(images, use.names = FALSE))
}

.islh_check_dir <- function(dir, call = rlang::caller_env()) {
  if (!dir.exists(dir)) {
    .islh_abort(
      c(
        "{.file {dir}} does not exist.",
        i = "Create it first, or use {.fn islh_create_report} to start a project."
      ),
      call = call
    )
  }
  invisible(TRUE)
}
