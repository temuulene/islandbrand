new_project <- function(format = "html") {
  path <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(path, "report")
  suppressMessages(islh_create_report(path, format = format))
  path
}

test_that("a freshly created project is complete and recorded", {
  project <- new_project()

  check <- islh_check_project(project, quiet = TRUE)
  expect_s3_class(check, "islh_project_check")
  expect_equal(names(check), c("path", "status"))
  expect_true(all(check$status == "current"))
  expect_true(file.exists(file.path(project, "_islh-manifest.csv")))

  manifest <- utils::read.csv(file.path(project, "_islh-manifest.csv"))
  expect_setequal(manifest$path, check$path)
  expect_equal(unique(manifest$version), as.character(islh_version()))
})

test_that("check separates a missing file from an edited one", {
  project <- new_project()
  check <- islh_check_project(project, quiet = TRUE)
  extension <- check$path[grepl("^_extensions", check$path)][1]

  unlink(file.path(project, extension))
  writeLines("a local change", file.path(project, "_brand.yml"))

  after <- islh_check_project(project, quiet = TRUE)
  expect_equal(after$status[after$path == extension], "missing")
  expect_equal(after$status[after$path == "_brand.yml"], "modified")
})

test_that("a project with no manifest cannot tell edited from old", {
  project <- new_project()
  unlink(file.path(project, "_islh-manifest.csv"))
  writeLines("a local change", file.path(project, "_brand.yml"))

  check <- islh_check_project(project, quiet = TRUE)

  # Without a record of what was installed, an edit and an old version look
  # the same, so the honest answer is that it cannot be verified.
  expect_equal(check$status[check$path == "_brand.yml"], "unverified")
  expect_false(attr(check, "has_manifest"))
  # Files that still match the package are unambiguous either way.
  expect_true(any(check$status == "current"))
})

test_that("a dry run reports the work and writes nothing", {
  project <- new_project()
  extension <- islh_check_project(project, quiet = TRUE)$path[1]
  unlink(file.path(project, extension))
  stamp <- file.info(file.path(project, "_islh-manifest.csv"))$mtime

  result <- islh_update_project(project, dry_run = TRUE, quiet = TRUE)

  expect_equal(result$action[result$path == extension], "written")
  expect_false(file.exists(file.path(project, extension)))
  expect_equal(file.info(file.path(project, "_islh-manifest.csv"))$mtime, stamp)
  expect_false(dir.exists(file.path(project, "_islh-backup")))
})

test_that("an update restores missing files and leaves edits alone", {
  project <- new_project()
  extension <- islh_check_project(project, quiet = TRUE)$path[
    grepl("^_extensions", islh_check_project(project, quiet = TRUE)$path)
  ][1]
  unlink(file.path(project, extension))
  writeLines("a local change", file.path(project, "_brand.yml"))

  result <- islh_update_project(project, quiet = TRUE)

  expect_equal(result$action[result$path == extension], "written")
  expect_true(file.exists(file.path(project, extension)))

  expect_equal(result$action[result$path == "_brand.yml"], "protected")
  expect_equal(readLines(file.path(project, "_brand.yml")), "a local change")
})

test_that("an edited file stays flagged after an update", {
  project <- new_project()
  writeLines("a local change", file.path(project, "_brand.yml"))
  islh_update_project(project, quiet = TRUE)

  # The manifest must not adopt somebody's edit as ours, or the next update
  # would overwrite it without warning.
  check <- islh_check_project(project, quiet = TRUE)
  expect_equal(check$status[check$path == "_brand.yml"], "modified")

  # The row is carried forward, not rewritten: it still records what the
  # package installed, which is what makes the edit visible at all.
  manifest <- utils::read.csv(
    file.path(project, "_islh-manifest.csv"),
    colClasses = "character"
  )
  recorded <- manifest$hash[manifest$path == "_brand.yml"]
  expect_length(recorded, 1L)
  expect_false(
    identical(recorded, unname(tools::md5sum(file.path(project, "_brand.yml"))))
  )
})

test_that("force replaces protected files and backs them up first", {
  project <- new_project()
  writeLines("a local change", file.path(project, "_brand.yml"))

  result <- islh_update_project(project, force = TRUE, quiet = TRUE)
  expect_equal(result$action[result$path == "_brand.yml"], "written")
  expect_false(
    identical(readLines(file.path(project, "_brand.yml")), "a local change")
  )

  backups <- list.files(
    file.path(project, "_islh-backup"),
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(backups, 1L)
  expect_equal(readLines(backups), "a local change")

  expect_true(all(
    islh_check_project(project, quiet = TRUE)$status == "current"
  ))
})

test_that("backups can be turned off", {
  project <- new_project()
  writeLines("a local change", file.path(project, "_brand.yml"))

  islh_update_project(project, force = TRUE, backup = FALSE, quiet = TRUE)
  expect_false(dir.exists(file.path(project, "_islh-backup")))
})

test_that("a project needing nothing is left completely alone", {
  project <- new_project()
  result <- islh_update_project(project, quiet = TRUE)

  expect_true(all(result$action == "skipped"))
  expect_false(dir.exists(file.path(project, "_islh-backup")))
  expect_message(islh_update_project(project), "Nothing to update")
})

test_that("the scaffold ignores its own backup folder", {
  project <- new_project()
  expect_true(any(grepl(
    "_islh-backup",
    readLines(file.path(project, ".gitignore"))
  )))
})

test_that("project functions check their arguments", {
  project <- new_project()

  expect_error(islh_check_project(file.path(project, "nope")), "does not exist")
  expect_error(
    islh_update_project(project, dry_run = NA),
    "single TRUE or FALSE"
  )
  expect_error(
    islh_update_project(project, force = "yes"),
    "single TRUE or FALSE"
  )
  expect_error(islh_check_project(project, quiet = 1), "single TRUE or FALSE")
})

test_that("line endings do not count as an edit", {
  # Git on Windows often checks text out with CRLF endings. A project cloned
  # there must still read as current, or it can never be updated.
  project <- new_project()
  brand <- file.path(project, "_brand.yml")
  lines <- readLines(brand)
  writeBin(charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n")), brand)

  status <- islh_check_project(project, quiet = TRUE)
  expect_equal(status$status[status$path == "_brand.yml"], "current")

  # A real edit is still an edit.
  writeLines(c(lines, "# local change"), brand)
  status <- islh_check_project(project, quiet = TRUE)
  expect_equal(status$status[status$path == "_brand.yml"], "modified")
})

test_that("binary files are hashed byte for byte", {
  # The Word reference document is a zip archive; stripping bytes from it
  # would hash a file that does not exist.
  docx <- islh_reference_docx()
  expect_equal(.islh_hash(docx), unname(tools::md5sum(docx)))
})

test_that("carriage returns without line feeds are hashed as they stand", {
  dir <- withr::local_tempdir()
  write_raw <- function(name, bytes) {
    path <- file.path(dir, name)
    writeBin(as.raw(bytes), path)
    path
  }
  md5 <- function(path) unname(tools::md5sum(path))
  text <- function(x) as.integer(charToRaw(x))

  first <- write_raw("first", text("first\rfile"))
  second <- write_raw("second", text("second\rfile"))
  # Both used to hash as an empty file.
  expect_equal(.islh_hash(first), md5(first))
  expect_equal(.islh_hash(second), md5(second))
  expect_false(.islh_hash(first) == .islh_hash(second))

  lone <- write_raw("lone", 13L)
  empty <- write_raw("empty", integer())
  expect_equal(.islh_hash(lone), md5(lone))
  expect_equal(.islh_hash(empty), md5(empty))
  expect_false(.islh_hash(lone) == .islh_hash(empty))

  # Mixed endings: only the CRLF pairs are normalized.
  mixed <- write_raw("mixed", text("a\r\nb\rc\n"))
  normalized <- write_raw("normalized", text("a\nb\rc\n"))
  expect_equal(.islh_hash(mixed), md5(normalized))

  # Binary content keeps every byte, carriage returns included.
  binary <- write_raw("binary", c(0L, 13L, 10L, 13L))
  expect_equal(.islh_hash(binary), md5(binary))
})

test_that("two updates in the same second keep separate backups", {
  project <- new_project()
  brand <- file.path(project, "_brand.yml")
  local_mocked_bindings(
    .islh_now = function() as.POSIXct("2026-01-05 09:30:00", tz = "UTC")
  )

  writeLines("first local edit", brand)
  islh_update_project(project, force = TRUE, quiet = TRUE)
  writeLines("second local edit", brand)
  islh_update_project(project, force = TRUE, quiet = TRUE)

  backups <- list.files(
    file.path(project, "_islh-backup"),
    pattern = "_brand.yml",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(backups, 2L)
  expect_setequal(
    vapply(backups, readLines, character(1), USE.NAMES = FALSE),
    c("first local edit", "second local edit")
  )
  expect_setequal(
    basename(dirname(backups)),
    c("20260105-093000", "20260105-093000-2")
  )
})

test_that("a backup never replaces an existing file", {
  dir <- withr::local_tempdir()
  from <- file.path(dir, "from")
  to <- file.path(dir, "to")
  writeLines("new", from)
  writeLines("old", to)
  expect_error(.islh_backup_copy(from, to), "Could not back up")
  expect_equal(readLines(to), "old")
})

test_that("an update that changes nothing leaves the manifest alone", {
  local_mocked_bindings(
    .islh_now = function() as.POSIXct("2026-01-05 09:30:00", tz = "UTC")
  )
  project <- new_project()
  manifest <- file.path(project, "_islh-manifest.csv")
  before <- readLines(manifest)

  local_mocked_bindings(
    .islh_now = function() as.POSIXct("2026-02-01 10:00:00", tz = "UTC")
  )
  islh_update_project(project, quiet = TRUE)
  expect_equal(readLines(manifest), before)

  # A replaced file gets a new row; the others keep theirs.
  unlink(file.path(project, "_brand.yml"))
  islh_update_project(project, quiet = TRUE)
  after <- utils::read.csv(manifest, colClasses = "character")
  expect_equal(after$written[after$path == "_brand.yml"], "2026-02-01 10:00:00")
  expect_true(all(
    after$written[after$path != "_brand.yml"] == "2026-01-05 09:30:00"
  ))
})
