test_that("epi hovers use reporting dates and group counts", {
  skip_if_not_installed("plotly")
  data <- data.frame(
    week = rep(as.Date("2025-12-28") + 7 * 0:1, 2),
    cases = c(1200, 5, 8, 10),
    area = rep(c("North", "South"), each = 2)
  )
  for (position in c("stack", "dodge")) {
    p <- islh_epi_curve(data, week, cases, fill = area, position = position)
    traces <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$data
    bars <- Filter(function(trace) identical(trace$type, "bar"), traces)
    north <- Filter(function(trace) identical(trace$name, "North"), bars)[[1]]
    expect_equal(
      as.character(north$hovertext),
      c(
        "Date: 2025-12-28<br>Cases: 1,200<br>area: North",
        "Date: 2026-01-04<br>Cases: 5<br>area: North"
      )
    )
    expect_equal(north$hovertemplate, "%{hovertext}<extra></extra>")
    expect_equal(as.numeric(north$y), c(1200, 5))
    separators <- Filter(function(trace) is.null(trace$name), traces)
    expect_equal(separators[[1]]$hoverinfo, "skip")
    expect_null(p$layers[[length(p$layers)]]$mapping$customdata)
  }
})

test_that("case tiles show counts instead of stacking positions", {
  skip_if_not_installed("plotly")
  data <- data.frame(week = as.Date("2026-01-04"), cases = 2)
  p <- islh_epi_curve(data, week, cases, style = "cases", labels = "total")
  traces <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$data
  tiles <- Filter(function(trace) identical(trace$fill, "toself"), traces)
  expect_length(tiles, 2L)
  expect_equal(
    unique(unlist(lapply(tiles, `[[`, "hovertext"))),
    "Date: 2026-01-04<br>Cases: 2"
  )
  totals <- Filter(function(trace) identical(trace$mode, "text"), traces)
  expect_equal(as.numeric(totals[[1]]$text), 2)
  expect_equal(totals[[1]]$hovertext, "Date: 2026-01-04<br>Total cases: 2")
})

test_that("reference hovers use original dates and readable field names", {
  skip_if_not_installed("plotly")
  data <- data.frame(week = as.Date("2026-01-04") + 7 * 0:1, cases = c(2, 4))
  reference <- data.frame(
    week = data$week,
    lower_limit = c(1, 2),
    upper_limit = c(5, 6),
    reference_mean = c(2.5, 3.5)
  )
  p <- islh_epi_curve(data, week, cases, reference = reference)
  traces <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$data
  text <- unlist(lapply(traces, `[[`, "hovertext"))
  expect_contains(text, "Date: 2026-01-04<br>Reference mean: 2.5")
  expect_contains(text, "Date: 2026-01-04<br>Lower limit: 1<br>Upper limit: 5")
  expect_equal(
    grepl("\\.islh_|group: 1|2026-01-07", paste(text, collapse = "")),
    FALSE
  )
})

test_that("hover labels respect plot labels and escape group values", {
  skip_if_not_installed("plotly")
  data <- data.frame(
    week = as.Date("2026-01-04"),
    cases = c(2, 3),
    area = c("<b>North</b>", NA_character_)
  )
  p <- islh_epi_curve(data, week, cases, fill = area) +
    ggplot2::labs(x = "Week starting", y = "Reported cases", fill = "Region")
  traces <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$data
  text <- unlist(lapply(traces, `[[`, "hovertext"))
  expect_contains(
    text,
    paste0(
      "Week starting: 2026-01-04<br>Reported cases: 2<br>",
      "Region: &lt;b&gt;North&lt;/b&gt;"
    )
  )
  expect_contains(
    text,
    "Week starting: 2026-01-04<br>Reported cases: 3<br>Region: Missing"
  )
})

test_that("tooltip choices work without losing extra layers or custom data", {
  skip_if_not_installed("plotly")
  data <- data.frame(week = as.Date("2026-01-04") + 7 * 0:1, cases = c(2, 4))
  p <- islh_epi_curve(data, week, cases)
  p$layers[[1]]$mapping$customdata <- ggplot2::aes(
    customdata = cases
  )$customdata
  p <- p +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(x = week, y = cases),
      inherit.aes = FALSE
    )
  traces <- plotly::plotly_build(islh_plotly(
    p,
    tooltip = "x",
    embed_fonts = FALSE
  ))$x$data
  expect_equal(as.character(traces[[1]]$hovertext), paste0("Date: ", data$week))
  expect_equal(as.numeric(traces[[1]]$customdata), data$cases)
  expect_match(traces[[2]]$text[[1]], "week: 2026-01-04", fixed = TRUE)
  traces <- plotly::plotly_build(islh_plotly(
    p,
    tooltip = "none",
    embed_fonts = FALSE
  ))$x$data
  expect_equal(traces[[1]]$hoverinfo, "skip")
})

test_that("hover preparation preserves reference and bar geometry", {
  skip_if_not_installed("plotly")
  data <- data.frame(
    week = rep(as.Date("2026-01-04") + 7 * 0:1, 2),
    cases = c(2, 4, 3, 5),
    area = rep(c("North", "South"), each = 2)
  )
  ref <- data.frame(
    week = unique(data$week),
    lower_limit = 1,
    upper_limit = 6,
    reference_mean = 3.5
  )
  for (style in c("bars", "cases")) {
    p <- islh_epi_curve(
      data,
      week,
      cases,
      fill = area,
      style = style,
      reference = ref,
      labels = "total"
    )
    prepared <- .islh_prepare_epi_hover(p, "all")$plot
    original <- ggplot2::ggplot_build(p)$data
    converted <- ggplot2::ggplot_build(prepared)$data
    for (i in seq_along(original)) {
      fields <- intersect(
        c("x", "y", "xmin", "xmax", "ymin", "ymax", "group", "fill"),
        names(original[[i]])
      )
      expect_equal(converted[[i]][fields], original[[i]][fields])
    }
  }
})

test_that("constant custom data is preserved for every row", {
  skip_if_not_installed("plotly")
  data <- data.frame(week = as.Date("2026-01-04") + 7 * 0:1, cases = c(2, 4))
  p <- islh_epi_curve(data, week, cases)
  p$layers[[1]]$mapping$customdata <- ggplot2::aes(
    customdata = "series"
  )$customdata
  trace <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$data[[1]]
  expect_equal(as.character(trace$customdata), rep("series", 2))
})

test_that("faceted hovers use aggregated counts and name missing panels", {
  skip_if_not_installed("plotly")
  data <- data.frame(
    week = as.Date("2026-01-04"),
    cases = c(2, 3, 0),
    area = c("North", "North", NA_character_)
  )
  p <- islh_epi_curve(data, week, cases, facet = area, aggregate = TRUE)
  traces <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$data
  text <- unlist(lapply(traces, `[[`, "hovertext"))
  expect_contains(text, "Date: 2026-01-04<br>Cases: 5<br>area: North")
  expect_contains(text, "Date: 2026-01-04<br>Cases: 0<br>area: Missing")
})

test_that("case fills carry a single label for browser area hovers", {
  skip_if_not_installed("plotly")
  data <- data.frame(
    week = as.Date("2026-01-04"),
    cases = c(2, 1),
    area = c("North", "South")
  )
  p <- islh_epi_curve(data, week, cases, fill = area, style = "cases")
  traces <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$data
  tiles <- Filter(function(trace) identical(trace$hoveron, "fills"), traces)
  expect_length(tiles, 3L)
  for (tile in tiles) {
    expect_length(tile$text, 1L)
    expect_match(
      tile$text,
      "Date: 2026-01-04<br>Cases: [12]<br>area: (North|South)"
    )
    expect_null(tile$hovertemplate)
  }
  traces <- plotly::plotly_build(islh_plotly(
    p,
    tooltip = "none",
    embed_fonts = FALSE
  ))$x$data
  tiles <- Filter(function(trace) identical(trace$hoveron, "fills"), traces)
  expect_true(all(vapply(
    tiles,
    function(trace) trace$hoverinfo == "skip",
    TRUE
  )))
})

test_that("reference numbers are rounded without padding other values", {
  skip_if_not_installed("plotly")
  data <- data.frame(week = as.Date("2026-01-04") + 7 * 0:1, cases = c(2, 4))
  reference <- data.frame(
    week = data$week,
    lower_limit = c(1, 1.25),
    upper_limit = c(5.123, 6),
    reference_mean = c(2.333333, 3)
  )
  p <- islh_epi_curve(data, week, cases, reference = reference)
  traces <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$data
  text <- unlist(lapply(traces, `[[`, "hovertext"))
  expect_contains(text, "Date: 2026-01-04<br>Reference mean: 2.33")
  expect_contains(text, "Date: 2026-01-11<br>Reference mean: 3")
  expect_contains(
    text,
    "Date: 2026-01-04<br>Lower limit: 1<br>Upper limit: 5.12"
  )
})

test_that("custom total labels read naturally", {
  skip_if_not_installed("plotly")
  data <- data.frame(week = as.Date("2026-01-04"), cases = 2)
  p <- islh_epi_curve(data, week, cases, labels = "total") +
    ggplot2::labs(y = "Number of cases")
  traces <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$data
  text <- unlist(lapply(traces, `[[`, "hovertext"))
  expect_contains(text, "Date: 2026-01-04<br>Number of cases (total): 2")
})
