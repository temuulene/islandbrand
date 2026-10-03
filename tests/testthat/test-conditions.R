# Errors raised by a shared helper must name the function the user called. A
# message headed "Error in `.islh_check_flag()`" points at code nobody wrote.

caller_of <- function(expr) {
  condition <- rlang::catch_cnd(expr, classes = "error")
  expect_s3_class(condition, "islh_error")
  rlang::call_name(condition$call)
}

test_that("validation errors name the exported function", {
  expect_equal(caller_of(theme_islh(base_size = -1)), "theme_islh")
  expect_equal(caller_of(theme_islh_map(legend_inside = 2)), "theme_islh_map")
  expect_equal(caller_of(islh_reset(quiet = NA)), "islh_reset")
  expect_equal(caller_of(islh_setup(quiet = NA)), "islh_setup")
})

test_that("epidemic curve input errors name islh_epi_curve()", {
  data <- data.frame(date = "not a date", count = 1)
  expect_equal(caller_of(islh_epi_curve(data, date, count)), "islh_epi_curve")

  data <- data.frame(date = as.Date("2026-01-01"), count = 1)
  expect_equal(
    caller_of(islh_epi_curve(data, date, missing_column)),
    "islh_epi_curve"
  )
  expect_equal(
    caller_of(
      islh_epi_curve(
        data,
        date,
        count,
        reference = data.frame(date = data$date, lower_limit = 0)
      )
    ),
    "islh_epi_curve"
  )
})

test_that("project errors name the exported function", {
  missing_dir <- file.path(tempdir(), "islandbrand-no-such-directory")
  expect_equal(caller_of(islh_use_quarto(missing_dir)), "islh_use_quarto")
  expect_equal(caller_of(islh_check_project(missing_dir)), "islh_check_project")
})

test_that("colour lookups raise classed errors", {
  expect_s3_class(
    rlang::catch_cnd(islh_brand("not-a-colour"), classes = "error"),
    "islh_error"
  )
  expect_s3_class(
    rlang::catch_cnd(islh_hex("blue", 51), classes = "error"),
    "islh_error"
  )
})
