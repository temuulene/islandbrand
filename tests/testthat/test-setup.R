# The nine blocks from the script's test suite, restated for a package. The
# properties still matter; how they are checked changes.

test_that("loading the package changes nothing", {
  # The script's version of this asserted that source() was silent. The rule is
  # the same and matters more now, because library(islandbrand) runs in every
  # report: loading must not probe fonts, set options, or touch the theme.
  # islh_setup() does all of that, when the user asks.
  expect_length(grep("^islh\\.", names(options())), 0L)

  # And it must not seed the font slot in the package's own state environment.
  # islh_font_family() treats that slot's existence as "already resolved", so
  # seeding it would make the package report a font it never looked for.
  #
  # The state has to be inspected in a session that has not run islh_setup(),
  # so this runs in a fresh R process rather than trusting the current one.
  script <- "cat(exists('font', envir = islandbrand:::.islh_state, inherits = FALSE))"
  result <- system2(
    file.path(R.home("bin"), "R"),
    c("--vanilla", "--slave", "-e", shQuote(script)),
    stdout = TRUE,
    stderr = FALSE
  )
  skip_if(length(result) == 0L, "could not start a fresh R session")
  expect_equal(trimws(paste(result, collapse = "")), "FALSE")
})

test_that("only the taught API is exported", {
  exports <- sort(getNamespaceExports("islandbrand"))

  # ih is Interior Health. The rename was a breaking change once; it should
  # not creep back.
  expect_length(grep("^ih_", exports), 0L)

  # Internals stay internal.
  expect_length(grep("^\\.islh_", exports), 0L)

  expect_snapshot(exports)
})

test_that("islh_help prints a grouped quick reference", {
  expect_snapshot(islh_help())
})

test_that("screen font registration degrades quietly off Windows", {
  # Registering the family removes Windows plot-pane warnings and is a no-op
  # everywhere else. An empty family name is never registered.
  expect_false(islandbrand:::.islh_register_screen_font(""))
  expect_silent(islandbrand:::.islh_register_screen_font("BC Sans"))
})

test_that("dependency checks are format-specific", {
  # A staff member writing HTML reports must never be made to install the Word
  # stack, and vice versa. This is the constraint the Imports/Suggests split
  # exists to serve.
  plots <- islh_check("plots", quiet = TRUE)
  expect_false(any(c("gt", "flextable", "officer") %in% plots$required))

  html <- islh_check("html", quiet = TRUE)
  expect_true("gt" %in% html$required)
  expect_true("base64enc" %in% html$required)
  expect_false(any(c("flextable", "officer") %in% html$required))

  docx <- islh_check("docx", quiet = TRUE)
  expect_true(all(c("flextable", "officer") %in% docx$required))
  expect_false("gt" %in% docx$required)

  # Turning tables off drops the table packages from every format.
  expect_false(
    "gt" %in% islh_check("html", tables = FALSE, quiet = TRUE)$required
  )
})

test_that("the install command never reaches for pak or a source build", {
  command <- islh_check("html", quiet = TRUE)$install_command
  expect_match(command, "install.packages(", fixed = TRUE)
  expect_match(command, 'type = "binary"', fixed = TRUE)
  expect_false(grepl("pak::", command, fixed = TRUE))
  expect_false(grepl('type = "source"', command, fixed = TRUE))
})

test_that("a package that is present but too old is reported as such", {
  # Found by running the code: the discrete scales need ggplot2 >= 3.5.0 and
  # islh_flextable() needs flextable >= 0.9.10. Without this gate both fail
  # inside the other package with a message that names neither.
  minimums <- islandbrand:::.islh_min_versions
  expect_true(all(c("ggplot2", "flextable") %in% names(minimums)))

  fake <- structure(
    list(
      ok = FALSE,
      format = "docx",
      tables = TRUE,
      embed_fonts = FALSE,
      required = "ggplot2",
      missing = character(),
      outdated = "ggplot2",
      install_command = 'install.packages(c("ggplot2"), type = "binary")'
    ),
    class = "islh_dependency_check"
  )

  # Not a snapshot: the message names the *installed* version, which differs
  # between machines, so a snapshot of it fails everywhere but where it was
  # recorded.
  message <- testthat::capture_messages(print(fake))
  expect_true(any(grepl("ggplot2", message)))
  expect_true(any(grepl("is too old", message)))
  expect_true(any(grepl(minimums[["ggplot2"]], message, fixed = TRUE)))
  expect_true(any(grepl("type = \"binary\"", message, fixed = TRUE)))
  expect_false(any(grepl("pak::", message, fixed = TRUE)))
})

test_that("plot setup is repeatable and leaves the session as it found it", {
  skip_if_not_installed("ggplot2")
  withr::local_options(list(
    islh.output_format = NULL,
    islh.embed_fonts = NULL,
    islh.document_webfont = NULL,
    ggplot2.discrete.colour = NULL,
    ggplot2.discrete.fill = NULL
  ))
  old_theme <- ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(old_theme))

  first <- expect_only_font_warnings(
    suppressMessages(islh_setup(format = "plots", quiet = TRUE))
  )
  second <- expect_only_font_warnings(
    suppressMessages(islh_setup(format = "plots", quiet = TRUE))
  )

  expect_equal(getOption("islh.output_format"), "plots")
  expect_equal(first$format, second$format)
  expect_equal(first$font, second$font)
})

test_that("HTML setup never initializes the Word table engine", {
  skip_if_not_installed("gt")
  withr::local_options(list(islh.output_format = NULL))
  old_theme <- ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(old_theme))

  # If HTML setup touched flextable, this stub would abort the call.
  local_mocked_bindings(
    .islh_set_flextable_defaults = function(...) {
      stop("HTML setup must not initialize flextable")
    }
  )

  expect_no_error(
    expect_only_font_warnings(
      suppressMessages(islh_setup(format = "html", quiet = TRUE))
    )
  )
})

test_that("gt font embedding is configurable and cached", {
  skip_if_not_installed("gt")
  skip_if_not_installed("base64enc")

  plain <- gt::as_raw_html(
    suppressWarnings(islh_gt(islh_example_data(), embed_fonts = FALSE))
  )
  expect_false(grepl("data:font/ttf;base64", plain, fixed = TRUE))

  # The base64 blob is expensive to build, so it is cached in .islh_state.
  # Without BC Sans installed the builder warns and caches an empty string;
  # either way the second call must return the first call's answer.
  first <- expect_only_font_warnings(islandbrand:::.islh_bc_sans_webfont_css())
  second <- expect_only_font_warnings(islandbrand:::.islh_bc_sans_webfont_css())
  expect_identical(first, second)
  expect_identical(first, islandbrand:::.islh_state$webfont_css)
})

test_that("the discrete scales use the current ggplot2 API", {
  # ggplot2 3.5.0 made discrete_scale()'s scale_name argument optional; these
  # constructors omit it. A deprecation warning here means the API moved again.
  expect_no_warning(scale_colour_islh())
  expect_no_warning(scale_fill_islh())
  expect_no_warning(scale_colour_islh_ordinal())
  expect_no_warning(scale_fill_islh_ordinal())
  expect_no_warning(scale_fill_islh_b())
})

test_that("islh_version reads DESCRIPTION rather than a second copy", {
  expect_equal(
    as.character(islh_version()),
    as.character(utils::packageVersion("islandbrand"))
  )
})

test_that("BC Sans is registered with every HTML render, not once a session", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("htmltools")
  local_mocked_bindings(
    .islh_bc_sans_webfont_css = function(...) "/* BC Sans */"
  )

  registered <- function() {
    meta <- knitr::knit_meta(clean = TRUE)
    any(vapply(
      meta,
      function(x) inherits(x, "html_dependency") && x$name == "islh-bc-sans",
      logical(1)
    ))
  }
  withr::defer(knitr::knit_meta(clean = TRUE))

  # Two renders in one session each start with empty knitr metadata. The
  # second must get the font too, or its tables fall back to another face.
  invisible(knitr::knit_meta(clean = TRUE))
  expect_true(.islh_register_webfont_dependency())
  expect_true(registered())

  expect_true(.islh_register_webfont_dependency())
  expect_true(registered())
})
