# The case style adds a blank layer before its tiles, so find them by geom.
tile_index <- function(plot) {
  which(vapply(plot$layers, function(l) inherits(l$geom, "GeomTile"), TRUE))
}
tile_data <- function(plot) plot$layers[[tile_index(plot)]]$data

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
  expect_equal(nrow(tile_data(plot)), 6)
  expect_equal(sort(tile_data(plot)$.islh_case_y[1:5]), seq(0.5, 4.5, 1))
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

  tile_plot <- islh_epi_curve(data, week, cases, style = "cases")
  tiles <- ggplot2::ggplot_build(tile_plot)$data[[tile_index(tile_plot)]]
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
  tiles <- tile_data(islh_epi_curve(
    data,
    date,
    count,
    fill = source,
    style = "cases"
  ))

  # First level on top, as geom_col() stacks bars: B fills the lower tiles.
  lowest <- tapply(tiles$.islh_case_y, tiles$source, min)
  expect_equal(lowest[["B"]], 0.5)
  expect_equal(lowest[["A"]], 2.5)
  per_date <- split(tiles[c("source", ".islh_case_y")], tiles$date)
  expect_equal(per_date[[1]], per_date[[2]], ignore_attr = TRUE)
})

test_that("aggregation keeps groups apart whatever their values contain", {
  # Labels pasted with a dot made ("A.B", "C") and ("A", "B.C") one group.
  data <- data.frame(
    date = as.Date("2026-01-04"),
    source = c("A.B", "A", "A.B"),
    site = c("C", "B.C", "C"),
    count = c(1, 10, 2)
  )
  plot <- islh_epi_curve(
    data,
    date,
    count,
    fill = source,
    facet = site,
    aggregate = TRUE
  )
  drawn <- plot$layers[[1]]$data
  expect_equal(drawn$source, c("A.B", "A"))
  expect_equal(drawn$site, c("C", "B.C"))
  expect_equal(drawn$count, c(3, 10))

  # Missing values are a group of their own, apart from the text "NA".
  missing <- data.frame(
    date = as.Date("2026-01-04"),
    source = c(NA, "NA", NA),
    count = c(1, 2, 4)
  )
  summed <- .islh_plot_grain(missing, "date", "count", "source", NULL, TRUE)
  expect_equal(summed$source, c(NA, "NA"))
  expect_equal(summed$count, c(5, 2))

  # Factors keep their levels.
  missing$source <- factor(missing$source, levels = c("NA", "Other"))
  summed <- .islh_plot_grain(missing, "date", "count", "source", NULL, TRUE)
  expect_equal(levels(summed$source), c("NA", "Other"))
  expect_equal(summed$count, c(5, 2))
})

test_that("case tiles keep zero-count periods, panels and fill groups", {
  data <- data.frame(
    date = as.Date("2026-01-04") + c(0, 7, 14, 0, 7, 14),
    count = c(0, 2, 0, 0, 0, 0),
    site = rep(c("Active", "Quiet"), each = 3),
    source = c("A", "A", "B", "B", "B", "B")
  )
  bars <- islh_epi_curve(data, date, count, fill = source, facet = site)
  tiles <- islh_epi_curve(
    data,
    date,
    count,
    fill = source,
    facet = site,
    style = "cases"
  )
  built_bars <- ggplot2::ggplot_build(bars)
  built_tiles <- ggplot2::ggplot_build(tiles)

  # The quiet panel stays.
  expect_equal(built_tiles$layout$layout$site, c("Active", "Quiet"))
  expect_equal(built_tiles$layout$layout$site, built_bars$layout$layout$site)

  # Leading and trailing zero weeks stay on the date axis.
  range_of <- function(built) built$layout$panel_params[[1]]$x$continuous_range
  expect_equal(range_of(built_tiles), range_of(built_bars))

  # B has no cases at all, and still appears in the legend.
  expect_equal(
    built_tiles$plot$scales$get_scales("fill")$get_limits(),
    c("A", "B")
  )
  expect_equal(nrow(tile_data(tiles)), 2L)
})

test_that("case tiles draw all-zero input without error", {
  data <- data.frame(
    date = as.Date("2026-01-04") + c(0, 7, 0, 7),
    count = 0,
    site = c("North", "North", "South", "South")
  )
  plot <- islh_epi_curve(data, date, count, facet = site, style = "cases")
  built <- ggplot2::ggplot_build(plot)
  expect_equal(built$layout$layout$site, c("North", "South"))
  expect_equal(nrow(tile_data(plot)), 0L)

  path <- withr::local_tempfile(fileext = ".png")
  grDevices::png(path)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(plot))

  single <- islh_epi_curve(data[1:2, ], date, count, style = "cases")
  expect_no_error(ggplot2::ggplot_build(single))
})

test_that("total labels keep a missing facet", {
  data <- data.frame(
    date = as.Date("2026-01-04"),
    site = c("Known", NA),
    count = c(2, 3)
  )
  plot <- expect_only_font_warnings(
    islh_epi_curve(data, date, count, facet = site, labels = "total")
  )
  built <- ggplot2::ggplot_build(plot)
  labels <- built$data[[2]]
  expect_equal(nrow(built$layout$layout), 2L)
  expect_equal(labels$label[order(labels$PANEL)], c(2, 3))

  totals <- .islh_plot_totals(data, "date", "count", "site")
  expect_equal(totals$site, c("Known", NA))
  expect_equal(totals$.islh_total, c(2, 3))
})

test_that("a reference needs one row per date, or per date and facet", {
  data <- data.frame(
    date = as.Date("2026-01-04") + c(0, 7),
    count = c(4, 5)
  )
  # A reference for each fill group would be joined into one zigzag line.
  stratified <- data.frame(
    date = rep(data$date, each = 2),
    source = rep(c("A", "B"), 2),
    reference_mean = c(1, 10, 2, 20)
  )
  expect_error(
    islh_epi_curve(data, date, count, reference = stratified),
    "more than one row for the same date",
    class = "islh_error"
  )

  # Dates that are equal once read count as the same date.
  as_text <- data.frame(
    date = c("2026-01-04", "2026-01-04"),
    reference_mean = c(1, 2)
  )
  as_text$date[2] <- format(as.Date("2026-01-04"))
  expect_error(
    islh_epi_curve(data, date, count, reference = as_text),
    "more than one row"
  )
})

test_that("a reference with the facet column has one row per panel", {
  data <- data.frame(
    date = rep(as.Date("2026-01-04") + c(0, 7), 2),
    site = rep(c("North", "South"), each = 2),
    count = c(4, 5, 1, 2)
  )
  by_site <- data.frame(
    date = data$date,
    site = data$site,
    reference_mean = c(3, 4, 1, 1)
  )
  plot <- islh_epi_curve(data, date, count, facet = site, reference = by_site)
  line <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(as.integer(table(line$PANEL)), c(2L, 2L))

  doubled <- rbind(by_site, by_site)
  expect_error(
    islh_epi_curve(data, date, count, facet = site, reference = doubled),
    "same date and facet"
  )

  misspelt <- by_site
  misspelt$site[3:4] <- "Sout"
  expect_error(
    islh_epi_curve(data, date, count, facet = site, reference = misspelt),
    "Sout"
  )

  # Without the facet column, one reference is drawn in every panel.
  shared <- data.frame(date = unique(data$date), reference_mean = c(2, 3))
  plot <- islh_epi_curve(data, date, count, facet = site, reference = shared)
  line <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(as.integer(table(line$PANEL)), c(2L, 2L))
})

test_that("a single period uses the interval islandepi records", {
  data <- data.frame(week = as.Date("2026-01-04"), cases = 3)
  bar_width <- function(plot) {
    built <- ggplot2::ggplot_build(plot)$data[[1]]
    built$xmax - built$xmin
  }
  expect_equal(bar_width(islh_epi_curve(data, week, cases)), 0.9)

  attr(data, "islh_interval") <- "week"
  expect_equal(bar_width(islh_epi_curve(data, week, cases)), 0.9 * 7)

  attr(data, "islh_interval") <- "month"
  data$week <- as.Date("2026-02-01")
  expect_equal(bar_width(islh_epi_curve(data, week, cases)), 0.9 * 28)

  attr(data, "islh_interval") <- "fortnight"
  expect_equal(bar_width(islh_epi_curve(data, week, cases)), 0.9)
})
