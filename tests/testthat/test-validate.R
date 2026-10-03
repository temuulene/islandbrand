test_that("flags reject anything that is not one TRUE or FALSE", {
  expect_true(.islh_check_flag(TRUE, "x"))
  expect_false(.islh_check_flag(FALSE, "x"))

  # isTRUE() alone would read every one of these as FALSE and carry on with
  # the option the caller did not ask for.
  expect_error(.islh_check_flag(NA, "x"), "single TRUE or FALSE")
  expect_error(.islh_check_flag(1, "x"), "single TRUE or FALSE")
  expect_error(.islh_check_flag("yes", "x"), "single TRUE or FALSE")
  expect_error(.islh_check_flag(c(TRUE, TRUE), "x"), "single TRUE or FALSE")
  expect_error(.islh_check_flag(NULL, "x"), "single TRUE or FALSE")
})

test_that("dimensions are positive inches within a page", {
  expect_equal(.islh_check_dimension(6.5, "width"), 6.5)

  expect_error(.islh_check_dimension(0, "width"), "positive")
  expect_error(.islh_check_dimension(-1, "width"), "positive")
  expect_error(.islh_check_dimension(NA_real_, "width"), "positive")
  expect_error(.islh_check_dimension(Inf, "width"), "positive")
  expect_error(.islh_check_dimension(c(1, 2), "width"), "positive")

  # The usual slip: a pixel count or a resolution passed where inches belong.
  expect_error(.islh_check_dimension(1200, "width"), "larger than any page")
})

test_that("dpi is a sane resolution", {
  expect_equal(.islh_check_dpi(300), 300)
  expect_error(.islh_check_dpi(0), "positive")
  expect_error(.islh_check_dpi(NA_real_), "positive")
  expect_error(.islh_check_dpi(1e6), "beyond what printing needs")
})

test_that("fractions are shares of the text width", {
  expect_equal(.islh_check_fraction(0.6, "width"), 0.6)
  expect_equal(.islh_check_fraction(1, "width"), 1)

  expect_error(.islh_check_fraction(0, "width"), "at most 1")
  expect_error(.islh_check_fraction(1.5, "width"), "at most 1")
  # 60 reads as a percentage, which is not what the argument means.
  expect_error(.islh_check_fraction(60, "width"), "at most 1")
})

test_that("sizes, counts and positions are checked", {
  expect_equal(.islh_check_size(12), 12)
  expect_error(.islh_check_size(0), "positive")
  expect_error(.islh_check_size(400), "no figure can show")

  expect_identical(.islh_check_count(50, "max_cases"), 50L)
  expect_error(.islh_check_count(2.5, "max_cases"), "whole number")
  expect_error(.islh_check_count(0, "max_cases"), "whole number")

  expect_equal(
    .islh_check_position(c(0.04, 0.16), "legend_inside"),
    c(0.04, 0.16)
  )
  expect_error(.islh_check_position(0.5, "legend_inside"), "two fractions")
  expect_error(
    .islh_check_position(c(0.5, 2), "legend_inside"),
    "two fractions"
  )
})

test_that("the public functions use the shared checks", {
  expect_error(theme_islh(base_size = -1), "positive")
  expect_error(theme_islh_map(base_size = 0), "positive")
  expect_error(
    theme_islh_map(legend_inside = c(2, 2)),
    "two fractions"
  )
  expect_error(islh_check(tables = NA), "single TRUE or FALSE")
  expect_error(islh_check(embed_fonts = "yes"), "single TRUE or FALSE")
  expect_error(
    islh_save_plot("figure.png", dpi = 0),
    "positive"
  )
  expect_error(
    islh_save_plot("figure.png", width = -2),
    "positive"
  )
})

test_that("table widths are validated in both engines", {
  skip_if_not_installed("gt")
  expect_error(islh_gt(datasets::mtcars, width = 60), "at most 1")
})

test_that("flextable widths and text widths are validated", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  expect_error(islh_flextable(datasets::mtcars, width = 0), "at most 1")
  expect_error(
    islh_flextable(datasets::mtcars, text_width = 0),
    "positive"
  )
  expect_error(
    islh_flextable(datasets::mtcars, autofit = NA),
    "single TRUE or FALSE"
  )
})
