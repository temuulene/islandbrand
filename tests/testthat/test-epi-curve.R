test_that("epi curve returns a branded ggplot", {
  data <- data.frame(
    date = as.Date("2026-01-01") + 0:4,
    count = c(0, 1, 3, 2, 1)
  )
  plot <- islh_epi_curve(data, date, count, title = "Example")
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Example")
  expect_equal(plot$labels$y, "Cases")
})

test_that("epi curve supports fill, facets and total labels", {
  data <- expand.grid(
    date = as.Date("2026-01-01") + 0:2,
    source = c("Community", "Facility"),
    region = c("North", "South")
  )
  data$count <- rep(c(1, 2, 3), 4)
  plot <- expect_only_font_warnings(
    islh_epi_curve(
      data,
      date,
      count,
      fill = source,
      facet = region,
      labels = "total"
    )
  )
  expect_s3_class(plot, "ggplot")
  expect_true(length(plot$layers) >= 2)
  expect_s3_class(plot$facet, "FacetWrap")
})

test_that("case style expands counts into individual rectangles", {
  data <- data.frame(
    date = as.Date(c("2026-01-01", "2026-01-01", "2026-01-02")),
    count = c(2, 3, 1),
    source = c("A", "B", "A")
  )
  plot <- islh_epi_curve(data, date, count, fill = source, style = "cases")
  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(plot$layers[[1]]$data), 6)
  expect_equal(sort(plot$layers[[1]]$data$.islh_case_y[1:5]), seq(0.5, 4.5, 1))
})

test_that("case style has a rendering guard", {
  data <- data.frame(date = as.Date("2026-01-01"), count = 100)
  expect_error(
    islh_epi_curve(data, date, count, style = "cases", max_cases = 10),
    "would draw"
  )
  expect_error(
    islh_epi_curve(data, date, count, style = "cases", position = "dodge"),
    "only supports"
  )
})

test_that("epi curve draws reference ribbons and lines", {
  data <- data.frame(
    period_start = as.Date("2026-01-01") + 0:4,
    count = c(1, 3, 4, 2, 1)
  )
  reference <- data.frame(
    period_start = data$period_start,
    lower_limit = rep(0, 5),
    upper_limit = rep(5, 5),
    reference_mean = rep(2.5, 5)
  )
  plot <- islh_epi_curve(
    data,
    period_start,
    count,
    reference = reference
  )
  expect_equal(length(plot$layers), 3)
  expect_s3_class(plot$layers[[1]]$geom, "GeomRibbon")
  expect_s3_class(plot$layers[[2]]$geom, "GeomLine")
})

test_that("epi curve validates columns, dates and counts", {
  data <- data.frame(date = "bad", count = 1)
  expect_error(islh_epi_curve(data, date, count), "invalid dates")

  data <- data.frame(date = as.Date("2026-01-01"), count = 1.5)
  expect_error(islh_epi_curve(data, date, count), "whole counts")
  expect_error(islh_epi_curve(data, missing, count), "not found")
  expect_error(
    islh_epi_curve(data, date, count, show_year_lines = NA),
    "TRUE or FALSE"
  )
})

test_that("epi curve validates reference inputs", {
  data <- data.frame(date = as.Date("2026-01-01") + 0:2, count = 1:3)
  one_limit <- data.frame(date = data$date, lower_limit = 0)
  expect_error(
    islh_epi_curve(data, date, count, reference = one_limit),
    "both lower and upper"
  )

  reversed <- data.frame(
    date = data$date,
    lower_limit = 4,
    upper_limit = 2
  )
  expect_error(
    islh_epi_curve(data, date, count, reference = reversed),
    "lower must not exceed"
  )
})

test_that("duplicated periods stop rather than stacking into one bar", {
  # geom_col() would add these into a single taller bar and the figure would
  # look finished while showing the wrong height.
  doubled <- data.frame(
    week = rep(as.Date("2026-01-05") + c(0, 7), each = 2),
    cases = c(1, 2, 3, 4)
  )

  expect_error(
    islh_epi_curve(doubled, week, cases),
    "more than one row for the same period"
  )
  expect_error(islh_epi_curve(doubled, week, cases), "islh_count_events")
})

test_that("aggregate = TRUE adds duplicated rows together", {
  doubled <- data.frame(
    week = rep(as.Date("2026-01-05") + c(0, 7), each = 2),
    cases = c(1, 2, 3, 4)
  )

  plot <- islh_epi_curve(doubled, week, cases, aggregate = TRUE)
  drawn <- plot$layers[[1]]$data

  expect_equal(nrow(drawn), 2L)
  expect_equal(drawn$cases, c(3, 7))
  expect_equal(drawn$week, as.Date("2026-01-05") + c(0, 7))
})

test_that("the grain is date, fill and facet together", {
  grain <- .islh_plot_grain

  # The same date in two fill groups is not a duplicate.
  by_fill <- data.frame(
    week = rep(as.Date("2026-01-05") + c(0, 7), each = 2),
    cases = c(1, 2, 3, 4),
    source = rep(c("A", "B"), 2)
  )
  expect_equal(
    nrow(grain(by_fill, "week", "cases", "source", NULL, FALSE)),
    4L
  )

  # Nor is the same date in two facets.
  by_facet <- data.frame(
    week = as.Date("2026-01-05"),
    cases = c(1, 2),
    site = c("North", "South")
  )
  expect_equal(
    nrow(grain(by_facet, "week", "cases", NULL, "site", FALSE)),
    2L
  )

  # Repeating one of those is.
  repeated <- rbind(by_facet, by_facet)
  expect_error(
    grain(repeated, "week", "cases", NULL, "site", FALSE),
    "more than one row"
  )
  summed <- grain(repeated, "week", "cases", NULL, "site", TRUE)
  expect_equal(summed$site, c("North", "South"))
  expect_equal(summed$cases, c(2, 4))
})

test_that("aggregate is checked like any other switch", {
  counts <- data.frame(week = as.Date("2026-01-05"), cases = 1)
  expect_error(
    islh_epi_curve(counts, week, cases, aggregate = NA),
    "single TRUE or FALSE"
  )
})

test_that("each bar spans its period from the start date", {
  # A weekly bar centred on its start date covers half of the week before and
  # crosses the year line at the wrong week.
  data <- data.frame(
    week = seq(as.Date("2025-12-21"), by = "week", length.out = 3),
    cases = c(2, 3, 1)
  )
  built <- ggplot2::ggplot_build(
    islh_epi_curve(data, week, cases, show_year_lines = FALSE)
  )$data[[1]]

  expect_equal(built$xmin, as.numeric(data$week))
  expect_equal(built$xmax - built$xmin, rep(0.9 * 7, 3))
})

test_that("case tiles, labels and reference follow the bars", {
  data <- data.frame(
    week = seq(as.Date("2026-01-04"), by = "week", length.out = 3),
    cases = c(2, 3, 1)
  )
  reference <- data.frame(
    week = data$week,
    lower_limit = 0,
    upper_limit = 4
  )
  middle <- as.numeric(data$week) + 0.9 * 7 / 2

  tiles <- ggplot2::ggplot_build(
    islh_epi_curve(data, week, cases, style = "cases")
  )$data[[1]]
  expect_equal(sort(unique(tiles$x)), middle)

  plot <- expect_only_font_warnings(
    islh_epi_curve(data, week, cases, labels = "total", reference = reference)
  )
  layers <- ggplot2::ggplot_build(plot)$data
  expect_equal(layers[[1]]$x, middle)
  expect_equal(layers[[length(layers)]]$x, middle)
})

test_that("date-times are counted on the date where they were recorded", {
  # 20:00 in Vancouver is 04:00 the next day in UTC. Before R 4.3, as.Date()
  # used UTC and moved these cases forward a day.
  times <- as.POSIXct(
    c("2026-01-05 20:00", "2026-01-06 09:00"),
    tz = "America/Vancouver"
  )
  data <- data.frame(time = times, cases = c(1, 2))
  plot <- islh_epi_curve(data, time, cases)

  expect_equal(
    plot$layers[[1]]$data$time,
    as.Date(c("2026-01-05", "2026-01-06"))
  )
})

test_that("case tiles stack in the same order in every period", {
  # Rows in a different order on each date used to swap the colours round.
  data <- data.frame(
    date = as.Date(c("2026-01-01", "2026-01-01", "2026-01-02", "2026-01-02")),
    source = c("A", "B", "B", "A"),
    count = c(1, 2, 2, 1)
  )
  tiles <- islh_epi_curve(
    data,
    date,
    count,
    fill = source,
    style = "cases"
  )$layers[[1]]$data

  # First level on top, as geom_col() stacks bars: B fills the lower tiles.
  lowest <- tapply(tiles$.islh_case_y, tiles$source, min)
  expect_equal(lowest[["B"]], 0.5)
  expect_equal(lowest[["A"]], 2.5)
  per_date <- split(tiles[c("source", ".islh_case_y")], tiles$date)
  expect_equal(per_date[[1]], per_date[[2]], ignore_attr = TRUE)
})
