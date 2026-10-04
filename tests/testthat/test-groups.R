test_that("group ids compare whole rows, not pasted labels", {
  data <- data.frame(
    a = c("A.B", "A", "A.B", "A"),
    b = c("C", "B.C", "C", "B.C")
  )
  expect_equal(.islh_group_ids(data, c("a", "b")), c(1L, 2L, 1L, 2L))
})

test_that("missing values match each other and nothing else", {
  data <- data.frame(
    site = c(NA, "NA", NA, ""),
    n = c(1, 1, 1, 1)
  )
  expect_equal(.islh_group_ids(data, "site"), c(1L, 2L, 1L, 3L))

  data$site <- factor(data$site)
  expect_equal(.islh_group_ids(data, "site"), c(1L, 2L, 1L, 3L))
})

test_that("group ids number groups in order of first appearance", {
  data <- data.frame(
    date = as.Date("2026-01-04") + c(7, 0, 7, 0),
    site = c("South", "North", "South", "South")
  )
  expect_equal(.islh_group_ids(data, c("date", "site")), c(1L, 2L, 1L, 3L))
  expect_equal(.islh_group_ids(data, character()), rep(1L, 4))
  expect_equal(.islh_group_ids(data[0, ], "site"), integer())
})

test_that("group sums follow the group ids", {
  ids <- c(2L, 1L, 2L, 3L)
  expect_equal(.islh_group_sums(c(1, 2, 3, 4), ids), c(2, 4, 4))
})
