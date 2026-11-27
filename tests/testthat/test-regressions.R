# Regression tests for defects found after the v0.1.0 release. Each test
# reproduces the original failure.

# Project files ----------------------------------------------------------------

test_that("written paths are relative to the project folder", {
  dir <- withr::local_tempdir()
  written <- suppressMessages(islh_use_brand(dir))
  expect_true(length(written) > 0L)
  expect_false(any(startsWith(written, "/")))
  expect_false(any(grepl("^[A-Za-z]:", written)))
  expect_false(any(grepl("\\", written, fixed = TRUE)))
  expect_true("_brand.yml" %in% written)
})

test_that("braces in a folder name are shown, not evaluated", {
  # A backup folder sits inside the project, so its full path, braces and
  # all, goes into the update message.
  project <- file.path(withr::local_tempdir(), "{draft}")
  suppressMessages(islh_create_report(project, example_data = FALSE))
  writeLines("a local change", file.path(project, "_brand.yml"))
  messages <- testthat::capture_messages(
    islh_update_project(project, force = TRUE)
  )
  expect_true(any(grepl("{draft}", messages, fixed = TRUE)))
  expect_true(any(grepl("_islh-backup", messages, fixed = TRUE)))

  written <- file.path(withr::local_tempdir(), "{brand}")
  dir.create(written)
  expect_no_error(suppressMessages(islh_use_brand(written)))
})

test_that("folder and flag arguments are checked", {
  expect_error(islh_use_quarto(NULL), "one folder path", class = "islh_error")
  expect_error(
    islh_use_quarto(c(tempdir(), tempdir())),
    "one folder path",
    class = "islh_error"
  )
  dir <- withr::local_tempdir()
  expect_error(
    islh_use_brand(dir, logos = NA),
    "TRUE or FALSE",
    class = "islh_error"
  )
  expect_error(
    islh_use_quarto(dir, overwrite = "yes"),
    "TRUE or FALSE",
    class = "islh_error"
  )
})

test_that("report scaffolding refuses titles that would break the YAML", {
  path <- file.path(withr::local_tempdir(), "report")
  expect_error(
    islh_create_report(path, title = c("a", "b")),
    "one non-empty string",
    class = "islh_error"
  )
  expect_error(
    islh_create_report(path, author = NA_character_),
    "NULL or one string",
    class = "islh_error"
  )
  expect_false(dir.exists(path))
})

test_that("a project without an RStudio file is not told to open one", {
  path <- file.path(withr::local_tempdir(), "no-rproj")
  messages <- testthat::capture_messages(
    islh_create_report(path, rproj = FALSE)
  )
  expect_false(any(grepl(".Rproj", messages, fixed = TRUE)))
  readme <- readLines(file.path(path, "README.md"))
  expect_false(any(grepl(".Rproj", readme, fixed = TRUE)))
})

test_that("the scaffolded report reads the CSV it ships with", {
  path <- file.path(withr::local_tempdir(), "csv-report")
  suppressMessages(islh_create_report(path))
  report <- readLines(file.path(path, "report.qmd"))
  expect_true(any(grepl(
    'read.csv("data/example-program-counts.csv")',
    report,
    fixed = TRUE
  )))
  expect_true(file.exists(file.path(path, "data", "example-program-counts.csv")))
})

test_that("a folder where a project file belongs stops with a message", {
  path <- file.path(withr::local_tempdir(), "folder-asset")
  suppressMessages(islh_create_report(path, example_data = FALSE))
  unlink(file.path(path, "_brand.yml"))
  dir.create(file.path(path, "_brand.yml"))
  expect_error(
    islh_check_project(path),
    "is a folder",
    class = "islh_error"
  )
})

# Fonts and tables -------------------------------------------------------------

test_that("embedded fonts declare the format of the file they carry", {
  expect_equal(.islh_font_kind("a/BCSans-Regular.woff2")$mime, "font/woff2")
  expect_equal(.islh_font_kind("BCSans.WOFF")$format, "woff")
  expect_equal(.islh_font_kind("BCSans.otf")$format, "opentype")
  expect_equal(.islh_font_kind("BCSans.ttf")$mime, "font/ttf")
})

test_that("islh_gt keeps the caller's CSS and adds the font once", {
  skip_if_not_installed("gt")
  local_mocked_bindings(
    .islh_bc_sans_webfont_css = function(...) "/* BC Sans */",
    .islh_document_has_webfont = function() FALSE
  )
  table <- gt::opt_css(gt::gt(data.frame(a = 1)), css = ".mine { color: red; }")
  styled <- suppressWarnings(islh_gt(islh_gt(table)))
  css <- .islh_gt_css(styled)
  expect_true(any(grepl(".mine", css, fixed = TRUE)))
  expect_equal(sum(css == "/* BC Sans */"), 1L)
})

test_that("embed_fonts = FALSE holds for gtsummary tables under setup", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  local_clean_session()
  local_mocked_bindings(
    .islh_bc_sans_webfont_css = function(...) "/* BC Sans */",
    .islh_document_has_webfont = function() FALSE
  )
  expect_only_font_warnings(
    suppressMessages(islh_setup(format = "html", quiet = TRUE))
  )
  summary <- gtsummary::tbl_summary(gtsummary::trial, include = age)
  styled <- islh_gtsummary_gt(summary, embed_fonts = FALSE)
  expect_false("/* BC Sans */" %in% .islh_gt_css(styled))
  expect_true(getOption("islh.embed_fonts"))
})

test_that("tables refuse input that is not a table or data frame", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  expect_error(islh_gt(1:3), "gt table or a data frame", class = "islh_error")
  expect_error(
    islh_flextable(1:3),
    "flextable or a data frame",
    class = "islh_error"
  )
})

test_that("flextable groups gather unsorted rows and shade a missing group", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  # Two missing values apart from each other still share one heading.
  data <- data.frame(g = c("A", NA, "B", "A", NA), v = 1:5)
  table <- suppressWarnings(islh_flextable(data, groupname_col = "g"))
  shown <- table$body$dataset
  headings <- is.na(shown$v)
  expect_equal(shown$g[headings], c("A", NA, "B"))
  expect_equal(shown$v[!headings], c(1L, 4L, 2L, 5L, 3L))
  # Every heading row, including the missing-value group, is shaded.
  shading <- table$body$styles$cells$background.color$data[, 1]
  expect_equal(length(unique(shading[headings])), 1L)
  expect_false(shading[headings][1] == shading[!headings][1])
})

# Epidemic curves --------------------------------------------------------------

test_that("totals need stacked bars when groups are dodged", {
  data <- data.frame(
    date = as.Date("2026-01-05") + 7 * rep(0:1, each = 2),
    n = c(3, 4, 5, 6),
    g = rep(c("a", "b"), 2)
  )
  expect_error(
    islh_epi_curve(data, date, n, fill = g, position = "dodge", labels = "total"),
    "needs stacked bars",
    class = "islh_error"
  )
  # Without groups, a dodged bar is the whole period, so totals still work.
  expect_s3_class(
    islh_epi_curve(
      data[c(1, 3), ],
      date,
      n,
      position = "dodge",
      labels = "total"
    ),
    "ggplot"
  )
})

test_that("a missing date or count is reported by name", {
  data <- data.frame(date = as.Date("2026-01-05"), n = 1)
  expect_error(islh_epi_curve(data), "`date` must select", class = "islh_error")
  expect_error(
    islh_epi_curve(data, date),
    "`count` must select",
    class = "islh_error"
  )
})

test_that("a reference column named by the caller must exist", {
  data <- data.frame(date = as.Date("2026-01-05") + 7 * 0:3, n = 1:4)
  reference <- data.frame(date = data$date, lower_limit = 0, upper_limit = 5)
  expect_error(
    islh_epi_curve(
      data,
      date,
      n,
      reference = reference,
      reference_date = "date",
      reference_mean = "mean_typo"
    ),
    "mean_typo",
    class = "islh_error"
  )
  # The default names may be absent.
  expect_s3_class(
    islh_epi_curve(data, date, n, reference = reference, reference_date = "date"),
    "ggplot"
  )
})

# Captions, sizes and counts ----------------------------------------------------

test_that("captions end each part with one full stop", {
  caption <- islh_caption(
    source = "BC Data.",
    extracted = "2026-01-01",
    boundary = "LHA 2024.",
    width = Inf
  )
  expect_identical(caption, "Source: BC Data. Extracted 2026-01-01. LHA 2024.")
})

test_that("caption arguments are checked", {
  expect_error(islh_caption("", "2026-01-01"), "one string", class = "islh_error")
  expect_error(
    islh_caption(Sys.Date(), "2026-01-01"),
    "one string",
    class = "islh_error"
  )
  expect_error(
    islh_caption("x", "2026-01-01", suppression = c("a", "b")),
    "suppression",
    class = "islh_error"
  )
  expect_error(
    islh_caption("x", "2026-01-01", width = 0),
    "width",
    class = "islh_error"
  )
})

test_that("label sizes are described in millimetres", {
  expect_error(islh_area_labels(size = -1), "millimetres", class = "islh_error")
  expect_error(
    islh_area_labels(size = 50),
    "4 is the Island Health default",
    class = "islh_error"
  )
})

test_that("counts outside the integer range and odd palette sizes are refused", {
  expect_error(.islh_check_count(1e12, "x"), "positive whole", class = "islh_error")
  expect_error(.islh_pal_qualitative(Inf), "positive whole", class = "islh_error")
  expect_error(.islh_pal_qualitative("3"), "positive whole", class = "islh_error")
  expect_error(.islh_pal_ordinal(2.5), "positive whole", class = "islh_error")
})

# Dependencies and session state ------------------------------------------------

test_that("installing for both formats reports every package still missing", {
  html <- list(
    ok = FALSE,
    format = "html",
    required = c("ggplot2", "gt"),
    missing = "gt",
    outdated = character()
  )
  docx <- list(
    ok = FALSE,
    format = "docx",
    required = c("ggplot2", "flextable", "officer"),
    missing = c("flextable", "officer"),
    outdated = character()
  )
  merged <- .islh_merge_checks(html, docx)
  expect_false(merged$ok)
  expect_equal(merged$missing, c("gt", "flextable", "officer"))
  expect_match(merged$install_command, "flextable", fixed = TRUE)
  expect_match(merged$install_command, "gt", fixed = TRUE)
})

test_that("capturing the session does not load the table packages", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    skip("needs the package source")
  }
  script <- paste(
    sprintf("suppressMessages(pkgload::load_all('%s', quiet = TRUE))", root),
    "invisible(islandbrand:::.islh_capture_state())",
    "cat(isNamespaceLoaded('flextable'), isNamespaceLoaded('gtsummary'))",
    sep = "; "
  )
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(script)),
    stdout = TRUE,
    stderr = FALSE
  ))
  expect_true(any(grepl("FALSE FALSE", output, fixed = TRUE)))
})

test_that("reset leaves table engines setup did not configure", {
  skip_if_not_installed("flextable")
  local_clean_session()
  expect_only_font_warnings(
    suppressMessages(islh_setup(format = "plots", quiet = TRUE))
  )
  record <- islandbrand:::.islh_state$setup
  expect_null(record$flextable)
  expect_null(record$gtsummary)

  # A setting made after a plot-only setup survives islh_reset().
  old <- flextable::set_flextable_defaults(font.size = 17)
  withr::defer(do.call(flextable::set_flextable_defaults, old))
  suppressMessages(islh_reset())
  expect_equal(flextable::get_flextable_defaults()$font.size, 17)

  # So does one made inside a plot-only with_islh() block.
  expect_only_font_warnings(with_islh(
    flextable::set_flextable_defaults(font.size = 19),
    format = "plots"
  ))
  expect_equal(flextable::get_flextable_defaults()$font.size, 19)
})

test_that("reset still restores the Word table defaults setup changed", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  local_clean_session()
  old <- flextable::set_flextable_defaults(font.size = 17)
  withr::defer(do.call(flextable::set_flextable_defaults, old))
  expect_only_font_warnings(
    suppressMessages(islh_setup(format = "docx", quiet = TRUE))
  )
  expect_equal(flextable::get_flextable_defaults()$font.size, 10)
  suppressMessages(islh_reset())
  expect_equal(flextable::get_flextable_defaults()$font.size, 17)

  # A Word setup after a plot-only one is still undone by reset.
  local_clean_session()
  expect_only_font_warnings(
    suppressMessages(islh_setup(format = "plots", quiet = TRUE))
  )
  expect_only_font_warnings(
    suppressMessages(islh_setup(format = "docx", quiet = TRUE))
  )
  suppressMessages(islh_reset())
  expect_equal(flextable::get_flextable_defaults()$font.size, 17)
})

test_that("examples restore the font and leave no variable behind", {
  example_file <- testthat::test_path("..", "..", "man", "theme_islh.Rd")
  if (!file.exists(example_file)) {
    skip("needs the package source")
  }
  code <- withr::local_tempfile(fileext = ".R")
  tools::Rd2ex(tools::parse_Rd(example_file), code)
  state <- islandbrand:::.islh_state
  had_font <- exists("font", envir = state, inherits = FALSE)
  old_font <- if (had_font) state$font
  withr::defer(
    if (had_font) {
      assign("font", old_font, envir = state)
    } else if (exists("font", envir = state, inherits = FALSE)) {
      rm("font", envir = state)
    }
  )
  assign("font", "Test Sans", envir = state)
  invisible(utils::capture.output(sys.source(code, envir = globalenv())))
  expect_equal(state$font, "Test Sans")
  expect_false(exists(".islh_example_font", envir = globalenv()))
})
