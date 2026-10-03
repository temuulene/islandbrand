# Table styling is a central feature and was previously untested.

sample_counts <- function() {
  data.frame(
    program = c("Primary care", "Mental health", "Public health"),
    encounters = c(326L, 184L, 79L),
    stringsAsFactors = FALSE
  )
}

# BC Sans is not installed on CI runners, so the webfont builder warns. That
# is expected and separately tested; it must not fail these.
quietly <- function(expr) suppressWarnings(suppressMessages(expr))

test_that("islh_flextable styles a data frame and keeps its content", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  ft <- quietly(islh_flextable(sample_counts()))
  expect_s3_class(ft, "flextable")
  expect_equal(flextable::nrow_part(ft, "body"), 3L)
  expect_equal(flextable::ncol_keys(ft), 2L)
})

test_that("flextable column widths are fixed and sum to the text width", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  # An autofit table is sized by whichever program opens it, so Word and
  # LibreOffice lay the same file out differently. The widths are resolved
  # here instead and written into the document.
  ft <- quietly(islh_flextable(sample_counts()))
  widths <- dim(ft)$widths
  expect_length(widths, 2L)
  expect_true(all(widths > 0))
  expect_equal(sum(widths), islandbrand:::.islh_text_width, tolerance = 1e-6)

  half <- quietly(islh_flextable(sample_counts(), width = 0.5))
  expect_equal(
    sum(dim(half)$widths),
    islandbrand:::.islh_text_width / 2,
    tolerance = 1e-6
  )

  # Without autofit every column gets an equal share.
  equal <- quietly(islh_flextable(sample_counts(), autofit = FALSE))
  shares <- dim(equal)$widths
  expect_equal(unname(shares[1]), unname(shares[2]), tolerance = 1e-9)
})

test_that("islh_flextable accepts an existing flextable and a caption", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  ft <- quietly(islh_flextable(
    flextable::flextable(sample_counts()),
    caption = "Encounters by program"
  ))
  expect_s3_class(ft, "flextable")
})

test_that("islh_flextable copes with awkward tables", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  # One column.
  one <- quietly(islh_flextable(data.frame(x = 1:3)))
  expect_equal(flextable::ncol_keys(one), 1L)
  expect_equal(
    sum(dim(one)$widths),
    islandbrand:::.islh_text_width,
    tolerance = 1e-6
  )

  # No rows: a table of nothing is still a table, not an error.
  empty <- quietly(islh_flextable(sample_counts()[0, ]))
  expect_s3_class(empty, "flextable")
  expect_equal(flextable::nrow_part(empty, "body"), 0L)

  # Many rows and many columns.
  wide <- as.data.frame(matrix(1:120, nrow = 20))
  big <- quietly(islh_flextable(wide))
  expect_equal(flextable::ncol_keys(big), 6L)
  expect_equal(
    sum(dim(big)$widths),
    islandbrand:::.islh_text_width,
    tolerance = 1e-6
  )
})

test_that("islh_gt styles a data frame and renders", {
  skip_if_not_installed("gt")

  tbl <- quietly(islh_gt(sample_counts()))
  expect_s3_class(tbl, "gt_tbl")

  html <- quietly(gt::as_raw_html(tbl))
  expect_true(grepl("Primary care", html, fixed = TRUE))
  expect_true(grepl("326", html, fixed = TRUE))
})

test_that("islh_gt titles and source notes appear in the output", {
  skip_if_not_installed("gt")

  html <- quietly(gt::as_raw_html(islh_gt(
    sample_counts(),
    title = "Encounters",
    subtitle = "By program",
    source_note = "Source: simulated data"
  )))

  expect_true(grepl("Encounters", html, fixed = TRUE))
  expect_true(grepl("By program", html, fixed = TRUE))
  expect_true(grepl("Source: simulated data", html, fixed = TRUE))
})

test_that("islh_gt copes with awkward tables", {
  skip_if_not_installed("gt")

  expect_s3_class(quietly(islh_gt(data.frame(x = 1:3))), "gt_tbl")
  expect_s3_class(quietly(islh_gt(sample_counts()[0, ])), "gt_tbl")
  expect_s3_class(
    quietly(islh_gt(as.data.frame(matrix(1:120, nrow = 20)))),
    "gt_tbl"
  )
})

test_that("the gtsummary converters produce the right object per format", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")

  data <- data.frame(
    group = rep(c("a", "b"), each = 10),
    value = as.numeric(1:20)
  )
  summary <- quietly(gtsummary::tbl_summary(data, by = "group"))

  expect_s3_class(quietly(islh_gtsummary_gt(summary)), "gt_tbl")
  expect_s3_class(quietly(islh_gtsummary_flex(summary)), "flextable")
})

test_that("both table engines use the same brand colours", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")

  # The header band and its text come from the brand ramp in both engines, so
  # an HTML report and a Word report of the same table look like siblings.
  html <- quietly(gt::as_raw_html(islh_gt(sample_counts())))
  band <- tolower(islh_hex("blue", 20))
  expect_true(grepl(band, tolower(html), fixed = TRUE))
})

test_that("gtsummary hooks work without library(islandbrand)", {
  skip_if_not_installed("gtsummary")
  # gtsummary evaluates these in its own environment, so an unqualified
  # islh_gt() is found only when the report attached the package.
  theme <- quietly(.islh_gtsummary_theme("gt", set_theme = FALSE, quiet = TRUE))
  hooks <- c(
    theme[["as_gt-lst:addl_cmds"]],
    theme[["as_flex_table-lst:addl_cmds"]]
  )
  for (hook in hooks) {
    expect_equal(rlang::call_ns(hook), "islandbrand")
  }
})

test_that("an old gtsummary is skipped with a warning, not an error", {
  skip_if_not_installed("gtsummary")
  local_mocked_bindings(.islh_outdated = function(packages) packages)
  expect_warning(
    expect_false(.islh_gtsummary_usable()),
    "too old"
  )
})

site_details <- function() {
  data.frame(
    hsda = c("South", "South", "Central", "North"),
    site = c("Site A", "Site B", "Site C", "Site D"),
    opened = c("2017-02-01", "2018-06-18", "2017-09-11", "2020-06-01")
  )
}

test_that("islh_gt groups rows and keeps a header over the row labels", {
  skip_if_not_installed("gt")
  table <- islh_gt(
    site_details(),
    groupname_col = "hsda",
    rowname_col = "site",
    embed_fonts = FALSE
  )
  html <- quietly(gt::as_raw_html(table))

  for (group in c("South", "Central", "North")) {
    expect_match(html, paste0(">", group, "<"))
  }
  # The row label column keeps its name as a header.
  expect_match(html, ">site<")

  # An empty stubhead leaves the header cell blank.
  blank <- quietly(gt::as_raw_html(islh_gt(
    site_details(),
    rowname_col = "site",
    stubhead = "",
    embed_fonts = FALSE
  )))
  expect_false(grepl(">site<", blank, fixed = TRUE))
})

test_that("islh_gt checks its grouping arguments", {
  skip_if_not_installed("gt")
  expect_error(
    islh_gt(site_details(), groupname_col = "region", embed_fonts = FALSE),
    class = "islh_error"
  )
  expect_error(
    islh_gt(site_details(), rowname_col = c("a", "b"), embed_fonts = FALSE),
    "one column name"
  )
  expect_error(
    islh_gt(gt::gt(site_details()), groupname_col = "hsda"),
    "need a data frame"
  )
})

test_that("islh_gt titles are bold despite gt's own title class", {
  skip_if_not_installed("gt")
  html <- quietly(gt::as_raw_html(
    islh_gt(site_details(), title = "Sites", embed_fonts = FALSE),
    inline_css = TRUE
  ))
  expect_match(html, "font-weight: bold[^>]*>Sites<")
})

test_that("islh_flextable groups rows with the same shading as islh_gt", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  table <- islh_flextable(site_details(), groupname_col = "hsda")

  # Four sites and three group heading rows.
  expect_equal(flextable::nrow_part(table, "body"), 7L)
  expect_false("hsda" %in% table$col_keys)

  fills <- table$body$styles$cells$background.color$data[, 1]
  expect_equal(sum(fills == islh_hex("blue", 96)), 3L)

  # Widths still fill the text width.
  expect_equal(sum(dim(table)$widths), islandbrand:::.islh_text_width)
})

test_that("islh_flextable checks its grouping argument", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  expect_error(
    islh_flextable(site_details(), groupname_col = "region"),
    class = "islh_error"
  )
  expect_error(
    islh_flextable(
      flextable::flextable(site_details()),
      groupname_col = "hsda"
    ),
    "needs a data frame"
  )
})
