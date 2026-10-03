# BC Sans is licensed by the BC government and is not shipped with the
# package; CI runners have neither it nor the Noto Sans fallback. Setup and the
# webfont builder then warn, correctly. Tests that exercise those paths use
# this helper so the expected warning is handled rather than left to surface as
# an unhandled condition in the check log — and so an *unexpected* warning
# still fails the test.

expect_only_font_warnings <- function(expr) {
  warnings <- character()
  result <- withCallingHandlers(
    force(expr),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  unexpected <- warnings[
    !grepl("BC Sans|Noto Sans|font", warnings, ignore.case = TRUE)
  ]
  testthat::expect_equal(
    unexpected,
    character(),
    label = "unexpected warnings"
  )

  invisible(result)
}
