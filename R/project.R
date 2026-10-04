# Keeping a scaffolded project up to date.
#
# `islh_create_report()` copies the Quarto extension, `_brand.yml` and the
# logos into a project, because Quarto reads them from the project directory
# rather than from an R library. That copy then ages: a fix to the Word
# reference document reaches nobody who scaffolded before it.
#
# Re-copying blindly is not the answer either. People edit `_brand.yml` for a
# program's own colours and add things to the extension, and overwriting that
# without warning loses work with no way back.
#
# So the project records what was installed. Comparing three hashes — what the
# package ships, what was installed, and what is on disk now — separates a file
# that is merely out of date from one somebody has edited, and only the first
# is safe to replace.

.islh_manifest_file <- "_islh-manifest.csv"

.islh_manifest_path <- function(dir) file.path(dir, .islh_manifest_file)

# The clock, behind a function so tests can fix it.
.islh_now <- function() Sys.time()

# Hashes ignore line endings in text files. Git on Windows commonly checks
# text out with CRLF endings, so a project cloned there, or a package built on
# a Windows runner, would otherwise read every text file as edited and the
# update would never touch them. An LF file hashes exactly as it did before
# this normalization, so existing manifests stay valid.
.islh_hash <- function(paths) {
  if (length(paths) == 0L) {
    return(character())
  }
  vapply(paths, .islh_hash_one, character(1), USE.NAMES = FALSE)
}

.islh_hash_one <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  size <- file.size(path)
  if (is.na(size)) {
    return(NA_character_)
  }
  bytes <- readBin(path, "raw", n = size)
  carriage_return <- as.raw(13L)
  is_text <- !any(bytes == as.raw(0L))
  if (!is_text || !any(bytes == carriage_return)) {
    return(unname(tools::md5sum(path)))
  }

  # Drop a carriage return only where it is followed by a line feed. A file
  # with carriage returns but no CRLF pair is hashed as it stands: dropping
  # `-integer(0)` would select no bytes and hash every such file as empty.
  crlf <- which(
    bytes[-length(bytes)] == carriage_return &
      bytes[-1L] == as.raw(10L)
  )
  if (length(crlf) == 0L) {
    return(unname(tools::md5sum(path)))
  }
  normalized <- tempfile()
  on.exit(unlink(normalized), add = TRUE)
  writeBin(bytes[-crlf], normalized)
  unname(tools::md5sum(normalized))
}

# Every file this package installs into a project, and where it comes from
# inside the installed package. One list, so the check, the update and the
# manifest cannot disagree about what belongs to the project.
.islh_project_assets <- function() {
  extensions <- .islh_path("quarto", "_extensions")
  files <- list.files(
    extensions,
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  assets <- data.frame(
    path = file.path("_extensions", files),
    source = file.path(extensions, files),
    stringsAsFactors = FALSE
  )

  assets <- rbind(
    assets,
    data.frame(
      path = "_brand.yml",
      source = islh_brand_yml(),
      stringsAsFactors = FALSE
    )
  )

  logos <- .islh_brand_logo_files()
  if (length(logos) > 0L) {
    assets <- rbind(
      assets,
      data.frame(
        path = file.path("logos", logos),
        source = vapply(
          logos,
          function(x) .islh_path("logos", x),
          character(1)
        ),
        stringsAsFactors = FALSE
      )
    )
  }

  assets$package_hash <- .islh_hash(assets$source)
  rownames(assets) <- NULL
  assets
}

.islh_read_manifest <- function(dir) {
  path <- .islh_manifest_path(dir)
  if (!file.exists(path)) {
    return(NULL)
  }
  manifest <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character"),
    error = function(condition) NULL
  )
  if (is.null(manifest) || !all(c("path", "hash") %in% names(manifest))) {
    return(NULL)
  }
  manifest
}

.islh_manifest_columns <- c("path", "hash", "version", "written")

# A row says "this is what the package put here", so it may only ever record a
# hash the package actually shipped. Recording somebody's edit would make the
# next update overwrite it without warning.
.islh_manifest_entries <- function(assets, paths) {
  keep <- assets[assets$path %in% paths, , drop = FALSE]
  data.frame(
    path = keep$path,
    hash = keep$package_hash,
    version = rep(as.character(islh_version()), nrow(keep)),
    written = rep(format(.islh_now(), "%Y-%m-%d %H:%M:%S"), nrow(keep)),
    stringsAsFactors = FALSE
  )
}

# Rows an earlier install wrote for files this run left alone. Dropping them
# would lose the very thing that identifies a file as edited: without the old
# hash, an edit and an old version become indistinguishable again.
.islh_manifest_carried <- function(previous, paths) {
  if (is.null(previous)) {
    return(NULL)
  }
  carried <- previous[previous$path %in% paths, , drop = FALSE]
  if (nrow(carried) == 0L) {
    return(NULL)
  }
  for (column in .islh_manifest_columns) {
    if (!column %in% names(carried)) {
      carried[[column]] <- NA_character_
    }
  }
  carried[.islh_manifest_columns]
}

.islh_write_manifest <- function(dir, entries) {
  entries <- entries[order(entries$path), .islh_manifest_columns, drop = FALSE]
  rownames(entries) <- NULL
  utils::write.csv(entries, .islh_manifest_path(dir), row.names = FALSE)
  invisible(entries)
}

# current     on disk matches what the package ships; nothing to do.
# missing     not in the project at all.
# outdated    matches what was installed, and the package now ships something
#             different; safe to replace.
# modified    differs from both what was installed and what is shipped, so
#             somebody edited it.
# unverified  differs from what is shipped and there is no manifest saying what
#             was installed, so an edit and an old version cannot be told
#             apart. Treated as edited, which is the safe reading.
.islh_project_status <- function(dir) {
  assets <- .islh_project_assets()
  manifest <- .islh_read_manifest(dir)

  on_disk <- file.path(dir, assets$path)
  present <- file.exists(on_disk)
  project_hash <- rep(NA_character_, nrow(assets))
  project_hash[present] <- .islh_hash(on_disk[present])

  installed <- rep(NA_character_, nrow(assets))
  if (!is.null(manifest)) {
    installed <- manifest$hash[match(assets$path, manifest$path)]
  }

  status <- ifelse(
    !present,
    "missing",
    ifelse(
      project_hash == assets$package_hash,
      "current",
      ifelse(
        is.na(installed),
        "unverified",
        ifelse(project_hash == installed, "outdated", "modified")
      )
    )
  )

  out <- data.frame(
    path = assets$path,
    status = status,
    stringsAsFactors = FALSE
  )
  attr(out, "assets") <- assets
  attr(out, "dir") <- dir
  attr(out, "has_manifest") <- !is.null(manifest)
  out
}

.islh_status_bullets <- function(x) {
  counts <- table(factor(
    x$status,
    levels = c("current", "outdated", "missing", "modified", "unverified")
  ))
  wording <- c(
    current = "up to date",
    outdated = "out of date, and safe to replace",
    missing = "missing from the project",
    modified = "edited here, so {?it/they} will be left alone",
    unverified = "different from this version, with no record of what was
                  installed, so {?it/they} will be left alone"
  )
  symbol <- c(
    current = "v",
    outdated = "i",
    missing = "i",
    modified = "!",
    unverified = "!"
  )

  bullets <- character()
  for (name in names(counts)) {
    if (counts[[name]] == 0L) {
      next
    }
    bullets <- c(
      bullets,
      stats::setNames(
        paste0("{", counts[[name]], "} file{?s} ", wording[[name]], "."),
        symbol[[name]]
      )
    )
  }
  bullets
}

#' Check a report project's Island Health files against this package
#'
#' Compares the Quarto extension, `_brand.yml` and the logos in a project
#' against the versions this package ships, and reports which are up to date,
#' out of date, missing or edited locally. It reads only; use
#' [islh_update_project()] to act on the result.
#'
#' A project written by [islh_create_report()] or updated by
#' [islh_update_project()] carries a manifest recording what was installed.
#' That record is what separates a file that is merely out of date from one
#' somebody has edited. Without it, a file that differs from this version is
#' reported as `"unverified"`, because the two cannot be told apart.
#'
#' @param dir Project directory. Defaults to the working directory.
#' @param quiet Suppress the summary message.
#'
#' @return A data frame of class `islh_project_check` with one row per file,
#'   giving `path` and `status`. `status` is one of `"current"`, `"outdated"`,
#'   `"missing"`, `"modified"` or `"unverified"`.
#' @export
#'
#' @examples
#' project <- file.path(tempdir(), "check-example")
#' islh_create_report(project, format = "html")
#'
#' check <- islh_check_project(project, quiet = TRUE)
#' table(check$status)
islh_check_project <- function(dir = ".", quiet = FALSE) {
  .islh_check_dir(dir)
  quiet <- .islh_check_flag(quiet, "quiet")

  out <- .islh_project_status(dir)
  class(out) <- c("islh_project_check", "data.frame")

  if (!quiet) {
    print(out)
  }
  invisible(out)
}

#' @param x An `islh_project_check` object.
#' @param ... Ignored.
#' @rdname islh_check_project
#' @export
print.islh_project_check <- function(x, ...) {
  if (all(x$status == "current")) {
    .islh_inform(c(
      "v" = "This project's Island Health files match {.pkg islandbrand}
             {islh_version()}."
    ))
    return(invisible(x))
  }

  bullets <- .islh_status_bullets(x)
  if (!isTRUE(attr(x, "has_manifest"))) {
    bullets <- c(
      bullets,
      "i" = paste0(
        "This project has no {.file ",
        .islh_manifest_file,
        "}, so an edited ",
        "file cannot be told from an old one."
      )
    )
  }
  bullets <- c(
    bullets,
    "i" = "Preview the changes with
           {.code islh_update_project(dry_run = TRUE)}."
  )

  .islh_inform(bullets)
  invisible(x)
}

#' Update a report project's Island Health files
#'
#' Replaces the Quarto extension, `_brand.yml` and the logos in a project with
#' the versions this package ships, so an existing report picks up a format fix
#' without being scaffolded again.
#'
#' Files that were edited in the project are left alone. So is any file the
#' project has no record of installing, since an edit and an old version cannot
#' be told apart without one. Pass `force = TRUE` to replace those too; the
#' backup is what makes that recoverable.
#'
#' Every file that is replaced is copied first into a new folder under
#' `_islh-backup`, unless `backup = FALSE`. Each update gets its own folder,
#' named for the time it ran, and never writes into an earlier one, so every
#' backup keeps the files as they were before that update. A backup of a file
#' you edited is the only copy of that edit outside Git: the package cannot
#' make it again. To restore a file, copy it from the backup folder back to the
#' same place in the project.
#'
#' An update that changes no files leaves the manifest as it was, so a
#' project in Git shows no change.
#'
#' @param dir Project directory. Defaults to the working directory.
#' @param dry_run Report what would change and write nothing.
#' @param backup Copy each replaced file into a new folder under
#'   `_islh-backup` first.
#' @param force Also replace files that were edited locally, or that the
#'   project has no record of installing.
#' @param quiet Suppress the summary message.
#'
#' @return A data frame with one row per file, giving `path`, `status` and the
#'   `action` taken: `"written"`, `"skipped"` or `"protected"`. Returned
#'   invisibly.
#' @export
#'
#' @examples
#' project <- file.path(tempdir(), "update-example")
#' islh_create_report(project, format = "html")
#'
#' # Nothing to do in a project that was just created.
#' islh_update_project(project, dry_run = TRUE, quiet = TRUE)
islh_update_project <- function(
  dir = ".",
  dry_run = FALSE,
  backup = TRUE,
  force = FALSE,
  quiet = FALSE
) {
  .islh_check_dir(dir)
  dry_run <- .islh_check_flag(dry_run, "dry_run")
  backup <- .islh_check_flag(backup, "backup")
  force <- .islh_check_flag(force, "force")
  quiet <- .islh_check_flag(quiet, "quiet")

  status <- .islh_project_status(dir)
  assets <- attr(status, "assets")

  protected <- status$status %in% c("modified", "unverified")
  replace <- status$status %in% c("outdated", "missing") | (protected & force)

  action <- ifelse(
    replace,
    "written",
    ifelse(protected, "protected", "skipped")
  )

  backup_dir <- NULL
  backed_up <- character()

  if (!dry_run) {
    previous <- .islh_read_manifest(dir)
    targets <- file.path(dir, status$path)

    # Back up everything first, so a failed backup stops the update before
    # any file has been replaced.
    to_back_up <- which(replace & file.exists(targets))
    if (backup && length(to_back_up) > 0L) {
      backup_dir <- .islh_backup_dir(dir)
      for (i in to_back_up) {
        .islh_backup_copy(targets[i], file.path(backup_dir, status$path[i]))
      }
      backed_up <- status$path[to_back_up]
    }
    for (i in which(replace)) {
      .islh_copy(assets$source[i], targets[i])
    }

    # Record the files now known to hold what the package ships, and carry
    # forward whatever an earlier install recorded for the ones left alone. A
    # current file the manifest already records keeps its row, so an update
    # that changes nothing leaves the manifest as it was.
    current <- status$status == "current"
    recorded <- rep(FALSE, nrow(status))
    if (!is.null(previous)) {
      row <- match(status$path, previous$path)
      recorded <- current &
        !is.na(row) &
        !is.na(previous$hash[row]) &
        previous$hash[row] == assets$package_hash
    }
    entries <- .islh_manifest_entries(
      assets,
      status$path[replace | (current & !recorded)]
    )
    carried <- .islh_manifest_carried(
      previous,
      status$path[!(replace | current) | recorded]
    )
    if (!is.null(carried)) {
      entries <- rbind(entries, carried)
    }
    if (!.islh_same_manifest(entries, previous)) {
      .islh_write_manifest(dir, entries)
    }
  }

  out <- data.frame(
    path = status$path,
    status = status$status,
    action = action,
    stringsAsFactors = FALSE
  )

  if (!quiet) {
    .islh_report_update(out, dry_run, backup_dir, backed_up, force)
  }
  invisible(out)
}

# A new folder for this update's backups. The name is the time to the second;
# a second update within the same second gets a numbered suffix rather than
# sharing, and so overwriting, the first one's folder. dir.create() fails on
# a folder that exists, which is what makes the name this update's alone.
.islh_backup_dir <- function(dir, call = rlang::caller_env()) {
  root <- file.path(dir, "_islh-backup")
  .islh_mkdir(root, call = call)
  stamp <- format(.islh_now(), "%Y%m%d-%H%M%S")
  for (attempt in seq_len(1000L)) {
    name <- if (attempt == 1L) stamp else paste0(stamp, "-", attempt)
    candidate <- file.path(root, name)
    if (dir.create(candidate, showWarnings = FALSE)) {
      return(candidate)
    }
  }
  .islh_abort(
    "Could not create a new backup folder in {.file {root}}.",
    call = call
  )
}

# Copies a file into a backup, refusing to replace anything already there.
.islh_backup_copy <- function(from, to, call = rlang::caller_env()) {
  .islh_mkdir(dirname(to), call = call)
  if (file.exists(to) || !file.copy(from, to, overwrite = FALSE)) {
    .islh_abort(
      c(
        "Could not back up {.file {from}}.",
        i = "Nothing was replaced. Pass {.code backup = FALSE} only if you
             have another copy."
      ),
      call = call
    )
  }
  invisible(to)
}

.islh_same_manifest <- function(entries, previous) {
  if (is.null(previous) || !all(.islh_manifest_columns %in% names(previous))) {
    return(FALSE)
  }
  tidy <- function(x) {
    x <- x[order(x$path), .islh_manifest_columns, drop = FALSE]
    rownames(x) <- NULL
    x[] <- lapply(x, as.character)
    x
  }
  identical(tidy(entries), tidy(previous))
}

.islh_report_update <- function(out, dry_run, backup_dir, backed_up, force) {
  written <- sum(out$action == "written")
  protected <- sum(out$action == "protected")

  if (written == 0L && protected == 0L) {
    .islh_inform(c(
      "v" = "Nothing to update: this project matches {.pkg islandbrand}
             {islh_version()}."
    ))
    return(invisible(NULL))
  }

  bullets <- character()
  if (written > 0L) {
    bullets <- c(
      bullets,
      stats::setNames(
        paste0(
          if (dry_run) "Would replace {" else "Replaced {",
          written,
          "} file{?s}."
        ),
        "v"
      )
    )
  }
  if (protected > 0L) {
    bullets <- c(
      bullets,
      stats::setNames(
        paste0(
          "{",
          protected,
          "} file{?s} {?was/were} left alone because ",
          "{?it differs/they differ} from this version and {?was/were} not ",
          "installed by it."
        ),
        "!"
      )
    )
    if (!force) {
      bullets <- c(
        bullets,
        "i" = "Replace {cli::qty(protected)}{?it/them} too with {.code force = TRUE}."
      )
    }
  }
  if (length(backed_up) > 0L) {
    bullets <- c(
      bullets,
      "i" = paste0(
        "{cli::qty(",
        length(backed_up),
        ")}The previous file{?s} {?is/are} in ",
        "{.file ",
        backup_dir,
        "}."
      )
    )
  }
  if (dry_run) {
    bullets <- c(
      bullets,
      "i" = "This was a dry run; nothing was written."
    )
  }

  .islh_inform(bullets)
  invisible(NULL)
}
