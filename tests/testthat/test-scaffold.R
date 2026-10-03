# These carry over the rules the starter-kit tests used to enforce. The kits
# are gone, but the properties that made them work still matter: a scaffolded
# project has to render from wherever the user puts it.

scaffold <- function(format = "html", ...) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(dir, "example-report")
  suppressMessages(islh_create_report(path, format = format, ...))
  path
}

test_that("a scaffolded project has everything needed to render", {
  path <- scaffold("docx")

  expect_true(file.exists(file.path(path, "report.qmd")))
  expect_true(file.exists(file.path(path, "_quarto.yml")))
  expect_true(file.exists(file.path(path, "_brand.yml")))
  expect_true(file.exists(file.path(path, "README.md")))
  expect_true(file.exists(file.path(path, ".gitignore")))
  expect_true(file.exists(file.path(path, "example-report.Rproj")))
  expect_true(file.exists(
    file.path(path, "data", "example-program-counts.csv")
  ))
  expect_true(file.exists(
    file.path(path, "_extensions", "islh", "islh-report", "_extension.yml")
  ))
})

test_that("report.qmd uses the package, not a copied script", {
  qmd <- readLines(file.path(scaffold("html"), "report.qmd"))

  expect_true(any(grepl("library(islandbrand)", qmd, fixed = TRUE)))
  expect_true(any(grepl("islh_setup()", qmd, fixed = TRUE)))

  # The whole point of the package: no per-project copy of the theme.
  expect_false(any(grepl("source(", qmd, fixed = TRUE)))
  expect_false(any(grepl("island-health-ggplot-theme", qmd, fixed = TRUE)))

  # Relative parent paths break as soon as the project is moved or renamed.
  expect_false(any(grepl("../", qmd, fixed = TRUE)))

  # Figures need alt text to be accessible.
  expect_true(any(grepl("fig-alt:", qmd, fixed = TRUE)))
})

test_that("each format wires up its own table engine and output format", {
  html <- readLines(file.path(scaffold("html"), "report.qmd"))
  expect_true(any(grepl("islh_gt(", html, fixed = TRUE)))
  expect_false(any(grepl("islh_flextable(", html, fixed = TRUE)))

  docx <- readLines(file.path(scaffold("docx"), "report.qmd"))
  expect_true(any(grepl("islh_flextable(", docx, fixed = TRUE)))
  expect_false(any(grepl("islh_gt(", docx, fixed = TRUE)))

  # "both" has to choose at render time, since one file serves either format.
  both <- readLines(file.path(scaffold("both"), "report.qmd"))
  expect_true(any(grepl("islh.output_format", both, fixed = TRUE)))
  expect_true(any(grepl("islh_gt(", both, fixed = TRUE)))
  expect_true(any(grepl("islh_flextable(", both, fixed = TRUE)))
})

test_that("_quarto.yml names the formats the extension contributes", {
  expect_true(any(grepl(
    "islh-report-html",
    readLines(file.path(scaffold("html"), "_quarto.yml")),
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "islh-report-docx",
    readLines(file.path(scaffold("docx"), "_quarto.yml")),
    fixed = TRUE
  )))

  both <- readLines(file.path(scaffold("both"), "_quarto.yml"))
  expect_true(any(grepl("islh-report-html", both, fixed = TRUE)))
  expect_true(any(grepl("islh-report-docx", both, fixed = TRUE)))
})

test_that("optional pieces can be turned off", {
  path <- scaffold("html", example_data = FALSE, rproj = FALSE)
  expect_false(dir.exists(file.path(path, "data")))
  expect_length(list.files(path, pattern = "\\.Rproj$"), 0L)
})

test_that("title and author reach the front matter", {
  qmd <- readLines(file.path(
    scaffold("html", title = "Flu Season 2026", author = "PHASE team"),
    "report.qmd"
  ))
  expect_true(any(grepl('title: "Flu Season 2026"', qmd, fixed = TRUE)))
  expect_true(any(grepl('author: "PHASE team"', qmd, fixed = TRUE)))

  # A NULL author leaves the field out rather than writing an empty one.
  no_author <- readLines(file.path(scaffold("html"), "report.qmd"))
  expect_false(any(grepl("author:", no_author, fixed = TRUE)))
})

test_that("scaffolding refuses to write over an existing project", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "existing")
  dir.create(path)
  writeLines("keep me", file.path(path, "report.qmd"))

  expect_error(islh_create_report(path), "already has files")
  expect_equal(readLines(file.path(path, "report.qmd")), "keep me")

  suppressMessages(islh_create_report(path, overwrite = TRUE))
  expect_true(any(grepl(
    "library(islandbrand)",
    readLines(file.path(path, "report.qmd")),
    fixed = TRUE
  )))
})

test_that("the install helper never reaches for pak or a source build", {
  # Island Health laptops block programs run from a user library, which breaks
  # pak, and have no compiler. This is the constraint the whole install path
  # exists to respect.
  # The installing itself sits in a small wrapper so tests can replace it;
  # read both.
  source_text <- paste(
    c(deparse(islh_install_deps), deparse(.islh_install_packages)),
    collapse = "\n"
  )
  expect_false(grepl("pak::", source_text, fixed = TRUE))
  expect_true(grepl("binary", source_text, fixed = TRUE))
  expect_true(grepl("install.packages", source_text, fixed = TRUE))

  command <- islh_check("docx", quiet = TRUE)$install_command
  expect_match(command, "install.packages(", fixed = TRUE)
  expect_match(command, 'type = "binary"', fixed = TRUE)
  expect_false(grepl("pak::", command, fixed = TRUE))
})

test_that("Word figure captions stay with their figures", {
  path <- scaffold("docx")
  extension_path <- file.path(
    path,
    "_extensions",
    "islh",
    "islh-report",
    "_extension.yml"
  )
  extension <- readLines(extension_path)
  docx <- which(trimws(extension) == "docx:")

  expect_length(docx, 1L)
  expect_true(any(grepl(
    "fig-cap-location: top",
    extension[seq.int(docx + 1L, length(extension))],
    fixed = TRUE
  )))

  reference <- file.path(
    path,
    "_extensions",
    "islh",
    "islh-report",
    "islh-report-reference.docx"
  )
  extracted <- withr::local_tempdir()
  utils::unzip(reference, files = "word/styles.xml", exdir = extracted)
  styles <- paste(
    readLines(file.path(extracted, "word", "styles.xml"), warn = FALSE),
    collapse = ""
  )

  expect_true(grepl(
    'w:styleId="ImageCaption"(?:(?!</w:style>).)*<w:keepNext',
    styles,
    perl = TRUE
  ))
})

test_that("the RStudio project template builds a project", {
  # RStudio creates the directory, then calls this with the widget values:
  # strings for text and select inputs, TRUE or FALSE for the checkbox.
  path <- file.path(withr::local_tempdir(), "from-rstudio")
  dir.create(path)
  suppressMessages(islh_report_skeleton(
    path,
    format = "html",
    title = "Flu season",
    author = "",
    example_data = FALSE
  ))

  expect_true(file.exists(file.path(path, "report.qmd")))
  expect_false(file.exists(file.path(path, "data")))
  # RStudio writes its own .Rproj for a template project.
  expect_length(list.files(path, pattern = "\\.Rproj$"), 0L)

  report <- readLines(file.path(path, "report.qmd"))
  expect_true(any(grepl('title: "Flu season"', report, fixed = TRUE)))
  # A blank author box leaves the field out rather than writing it empty.
  expect_false(any(grepl("^author:", report)))
})
