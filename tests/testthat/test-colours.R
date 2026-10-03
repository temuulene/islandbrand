test_that("islh_hex returns the ramp value for a family and step", {
  expect_equal(islh_hex("blue", 50), "#007CC8")
  expect_length(islh_hex("blue", c(30, 50, 80)), 3L)

  # Family names are matched case-insensitively.
  expect_equal(islh_hex("BLUE", 50), islh_hex("blue", 50))
})

test_that("every family has the same twenty steps", {
  families <- names(islandbrand:::.islh_colours)
  expect_length(families, 8L)
  steps <- names(islandbrand:::.islh_colours[[1]])
  for (family in families) {
    expect_equal(names(islandbrand:::.islh_colours[[family]]), steps)
  }
})

test_that("an unknown step is named in the error, with the ones that exist", {
  # This message could never be built before: it interpolates two vectors, and
  # cli could not tell which one the plural marker referred to. The failure was
  # "Multiple quantities for pluralization" rather than anything useful.
  expect_error(islh_hex("grey", 49), '"49"')
  expect_error(islh_hex("grey", 49), "not defined for family")

  # Singular and plural both have to build.
  expect_error(islh_hex("blue", c(49, 51)), '"49" and "51"')
  expect_error(islh_hex("blue", c(49, 51)), "are not defined")
  expect_error(islh_hex("blue", 49), "is not defined")
})

test_that("an unknown family lists the available ones", {
  expect_error(islh_hex("mauve", 50), "not an Island Health colour family")
  expect_error(islh_hex("mauve", 50), "blue")
  expect_error(islh_hex(c("blue", "grey"), 50), "one non-missing")
})

test_that("islh_brand resolves the role names used in _brand.yml", {
  expect_equal(islh_brand("primary"), "#007CC8")
  expect_length(islh_brand(c("success", "warning", "danger")), 3L)
  expect_named(islandbrand:::.islh_brand_colours)
})

test_that("the missing-package message can actually be built", {
  # Same class of bug: cli interpolates in the caller's frame, so the wrapper
  # has to forward `.envir` or `{package}` resolves to nothing.
  expect_error(
    islandbrand:::.islh_require("notarealpackage", "a made-up feature"),
    "notarealpackage"
  )
  expect_error(
    islandbrand:::.islh_require("notarealpackage", "a made-up feature"),
    "a made-up feature"
  )
})

test_that("categorical colours stand out from a white page", {
  # WCAG 2.1 asks 3:1 for graphical objects. The qualitative palette carries
  # meaning through colour, so every category has to clear it on white.
  ratios <- .islh_contrast_ratio(.islh_pal_qualitative(5), "#FFFFFF")
  expect_true(
    all(ratios >= 3),
    label = paste(round(ratios, 2), collapse = ", ")
  )
})

test_that("table header text is readable on the header band", {
  # Both table engines print white bold text on blue 20. Body-size text needs
  # 4.5:1 under WCAG 2.1 AA.
  expect_gte(
    .islh_contrast_ratio(islh_brand("white"), islh_hex("blue", 20)),
    4.5
  )
})
