test_that("nothing but the data draws on a map", {
  map_theme <- theme_islh_map()

  # `calc_element()` resolves inheritance, so this asserts what actually
  # reaches the page rather than what the theme happens to set. An earlier
  # version blanked `axis.line` while `theme_islh()` set `axis.line.x`
  # directly, and a box and tick marks were drawn around every map.
  furniture <- c(
    "axis.line.x",
    "axis.line.y",
    "axis.ticks.x",
    "axis.ticks.y",
    "axis.text.x",
    "axis.text.y",
    "axis.title.x",
    "axis.title.y",
    "panel.grid.major",
    "panel.grid.minor",
    "panel.border"
  )

  for (element in furniture) {
    expect_s3_class(
      ggplot2::calc_element(element, map_theme),
      "element_blank"
    )
  }
})

test_that("the map theme keeps the brand text hierarchy", {
  map_theme <- theme_islh_map()

  expect_identical(
    ggplot2::calc_element("plot.title.position", map_theme),
    "plot"
  )
  expect_identical(ggplot2::calc_element("plot.title", map_theme)$face, "bold")
  expect_identical(ggplot2::calc_element("plot.caption", map_theme)$hjust, 0)
})

test_that("the map theme paints an opaque background", {
  # `theme_void()` leaves the background transparent, which reads as a broken
  # figure in Word and PowerPoint.
  background <- ggplot2::calc_element("plot.background", theme_islh_map())

  expect_identical(background$fill, "white")
})

test_that("the inside legend defaults to the empty top-right corner", {
  # The earlier default, c(0.04, 0.16), overlapped the west coast once real
  # boundaries were drawn. The top right is empty on a full-Island map.
  expect_equal(
    ggplot2::calc_element(
      "legend.position.inside",
      theme_islh_map(legend = "inside")
    ),
    c(0.62, 0.8)
  )
})

test_that("the legend can sit in the open water", {
  inside <- theme_islh_map(legend = "inside", legend_inside = c(0.1, 0.3))

  expect_identical(ggplot2::calc_element("legend.position", inside), "inside")
  expect_equal(
    ggplot2::calc_element("legend.position.inside", inside),
    c(0.1, 0.3)
  )
  expect_identical(
    ggplot2::calc_element("legend.position", theme_islh_map()),
    "bottom"
  )
  expect_identical(
    ggplot2::calc_element("legend.position", theme_islh_map(legend = "none")),
    "none"
  )
})

test_that("the map theme rejects a legend position that is not a pair", {
  expect_error(
    theme_islh_map(legend = "inside", legend_inside = 0.5),
    "two fractions"
  )
})

test_that("the map theme composes with the binned fill scale", {
  plot <- ggplot2::ggplot(
    data.frame(x = 1:4, y = 1:4, value = 1:4),
    ggplot2::aes(x, y, fill = value)
  ) +
    ggplot2::geom_tile() +
    scale_fill_islh_b() +
    ggplot2::coord_equal() +
    theme_islh_map()

  expect_s3_class(plot, "ggplot")
  expect_no_warning(ggplot2::ggplot_build(plot))
})
