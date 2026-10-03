test_that("map coordinates lock to BC Albers with no graticule", {
  skip_if_not_installed("sf")
  coords <- coord_islh_map()

  expect_s3_class(coords, "CoordSf")
  expect_equal(sf::st_crs(coords$crs), sf::st_crs(3005))
  expect_true(is.na(coords$datum))
  expect_false(coords$expand)
})

test_that("map coordinates accept an inset window", {
  skip_if_not_installed("sf")
  inset <- coord_islh_map(xlim = c(1150000, 1250000), ylim = c(350000, 420000))

  expect_s3_class(inset, "CoordSf")
})

test_that("the binned fill legend is wide enough to label", {
  legend <- .islh_legend_bar_theme()

  # `legend.key.width` sets the whole bar length for a horizontal colour bar.
  # At the 0.9-line qualitative key size the bar is shorter than its own break
  # labels, which is what made them overprint each other.
  expect_gt(as.numeric(legend$legend.key.width), 6)
  expect_identical(legend$legend.title.position, "top")
  expect_s3_class(legend$legend.ticks, "element_blank")
})

test_that("key_width lengthens the legend bar", {
  narrow <- .islh_legend_bar_theme()
  wide <- .islh_legend_bar_theme(key_width = 16)

  expect_gt(
    as.numeric(wide$legend.key.width),
    as.numeric(narrow$legend.key.width)
  )
})

test_that("binned fill breaks are abbreviated by default", {
  labels <- formals(scale_fill_islh_b)$labels |> eval()

  expect_identical(labels(c(1000, 25000, 1e6)), c("1K", "25K", "1M"))
})

test_that("the binned fill scale accepts a caller's labels and guide", {
  scale <- scale_fill_islh_b(
    labels = scales::label_comma(),
    guide = "none"
  )

  expect_identical(scale$labels(1e5), "100,000")
  expect_identical(scale$guide, "none")
})

test_that("areas with no data cannot be mistaken for the lowest bin", {
  bins <- .islh_lightness(.islh_pal_map())
  missing <- .islh_lightness(.islh_map_missing())

  # The categorical unknown grey matches the lightest bin in lightness, so a
  # greyscale print cannot tell no data from the lowest band. The map colour
  # has to sit clear of every bin.
  expect_gt(min(abs(missing - bins)), 10)
  expect_identical(
    eval(formals(scale_fill_islh_b)$na.value),
    .islh_map_missing()
  )
})

test_that("the map palette steps evenly in lightness", {
  steps <- abs(diff(.islh_lightness(.islh_pal_map())))

  # Even lightness steps are what keep a single-hue ramp readable in
  # greyscale and for colour-blind readers.
  expect_true(all(steps > 5))
  expect_lt(max(steps) - min(steps), 10)
})

test_that("islh_caption assembles only the parts it is given", {
  full <- islh_caption(
    source = "BC Data Catalogue",
    extracted = "2026-03-31",
    boundary = "Local health areas, 2024 boundaries",
    standard_pop = "2011 Canadian standard population",
    suppression = "Counts under 5 suppressed",
    governance = "Shared under the First Nations principles of OCAP",
    width = Inf
  )

  expect_match(full, "^Source: BC Data Catalogue\\. Extracted 2026-03-31\\.")
  expect_match(full, "Standardized to 2011 Canadian standard population\\.")
  expect_match(full, "Counts under 5 suppressed\\.")
  expect_match(full, "OCAP\\.$")

  spare <- islh_caption("BC Data Catalogue", "2026-03-31")
  expect_identical(spare, "Source: BC Data Catalogue. Extracted 2026-03-31.")
})

test_that("islh_caption wraps so it does not run off the figure", {
  full <- islh_caption(
    source = "BC Data Catalogue",
    extracted = "2026-03-31",
    boundary = "Local health areas, 2024 boundaries",
    standard_pop = "2011 Canadian standard population",
    suppression = "Counts under 5 suppressed"
  )
  lines <- strsplit(full, "\n", fixed = TRUE)[[1]]

  # ggplot2 draws a caption on one line and lets it run past the panel.
  expect_gt(length(lines), 1)
  expect_true(all(nchar(lines) <= 100))
  expect_false(grepl("\n", islh_caption("BC Stats", "2026-03-31", width = Inf)))
})

test_that("islh_caption takes a Date and rejects a vector", {
  dated <- islh_caption("BC Stats", as.Date("2026-03-31"))

  expect_match(dated, "Extracted 2026-03-31\\.")
  expect_error(islh_caption(c("a", "b"), "2026-03-31"), "one string")
})
