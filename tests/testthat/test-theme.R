test_that("count axis breaks fall on whole numbers", {
  # The default algorithm puts breaks at 2.5 and 7.5, which the whole-number
  # labels print as "2" and "8" beside the wrong gridlines.
  breaks <- .islh_count_breaks()
  for (upper in c(1.05, 3.15, 9.45, 26, 12600)) {
    b <- breaks(c(0, upper))
    expect_true(all(b == round(b)), label = paste("upper", upper))
    expect_gte(length(b), 2L)
  }
  expect_equal(breaks(c(0, 1.05)), c(0, 1))
})

test_that("an epidemic curve's count labels are distinct", {
  data <- data.frame(
    week = seq(as.Date("2026-01-04"), by = "week", length.out = 3),
    cases = c(0, 1, 1)
  )
  plot <- islh_epi_curve(data, week, cases)
  labels <- ggplot2::get_guide_data(plot, "y")$.label
  expect_false(anyDuplicated(labels) > 0)
})
