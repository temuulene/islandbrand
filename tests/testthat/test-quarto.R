test_that("islh_use_quarto writes a complete, parseable extension", {
  dir <- withr::local_tempdir()
  written <- suppressMessages(islh_use_quarto(dir))

  base <- file.path(dir, "_extensions", "islh", "islh-report")
  expect_true(file.exists(file.path(base, "_extension.yml")))
  expect_true(file.exists(file.path(base, "islh.scss")))
  expect_true(file.exists(file.path(base, "islh-report-reference.docx")))
  expect_length(written, 3L)

  skip_if_not_installed("yaml")
  ext <- yaml::read_yaml(file.path(base, "_extension.yml"))

  # Quarto resolves `format: islh-report-html` from the directory name plus the
  # base format, so both must be contributed.
  expect_named(ext$contributes$formats, c("common", "html", "docx"))

  # The reference doc is named relative to the extension directory; an absolute
  # or missing path silently drops the Island Health styling.
  expect_equal(
    ext$contributes$formats$docx$`reference-doc`,
    "islh-report-reference.docx"
  )
  expect_true(file.exists(file.path(
    base,
    ext$contributes$formats$docx$`reference-doc`
  )))

  # Likewise the SCSS layer.
  expect_true(
    "islh.scss" %in% unlist(ext$contributes$formats$html$theme)
  )
})

test_that("islh_use_quarto leaves existing files alone unless told otherwise", {
  dir <- withr::local_tempdir()
  suppressMessages(islh_use_quarto(dir))

  target <- file.path(dir, "_extensions", "islh", "islh-report", "islh.scss")
  writeLines("/* edited by hand */", target)

  suppressMessages(islh_use_quarto(dir))
  expect_equal(readLines(target), "/* edited by hand */")

  suppressMessages(islh_use_quarto(dir, overwrite = TRUE))
  expect_gt(length(readLines(target)), 1L)
})

test_that("islh_use_brand writes _brand.yml and respects overwrite", {
  dir <- withr::local_tempdir()
  suppressMessages(islh_use_brand(dir))

  target <- file.path(dir, "_brand.yml")
  expect_true(file.exists(target))

  writeLines("# edited", target)
  suppressMessages(islh_use_brand(dir))
  expect_equal(readLines(target), "# edited")

  suppressMessages(islh_use_brand(dir, overwrite = TRUE))
  expect_gt(length(readLines(target)), 1L)
})

test_that("the copy helpers refuse a directory that does not exist", {
  missing <- file.path(withr::local_tempdir(), "not-created")
  expect_error(islh_use_quarto(missing), "does not exist")
  expect_error(islh_use_brand(missing), "does not exist")
})

test_that("the extension version tracks the package version", {
  skip_if_not_installed("yaml")
  ext <- yaml::read_yaml(
    system.file(
      "quarto",
      "_extensions",
      "islh",
      "islh-report",
      "_extension.yml",
      package = "islandbrand"
    )
  )
  expect_equal(as.character(ext$version), as.character(islh_version()))
})
