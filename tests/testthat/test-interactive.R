test_that("interactive plots preserve data, scales and report labels", {
  skip_if_not_installed("plotly")
  p <- ggplot2::ggplot(
    data.frame(week = 1:3, cases = c(10, 20, 30)),
    ggplot2::aes(week, cases)
  ) +
    ggplot2::geom_col(fill = islh_brand("primary")) +
    ggplot2::labs(
      title = "Weekly cases",
      subtitle = "Simulated data",
      caption = "Source: example",
      x = "Week",
      y = "Cases"
    )
  widget <- islh_plotly(p, height = 400, embed_fonts = FALSE)
  built <- plotly::plotly_build(widget)

  expect_s3_class(widget, "plotly")
  expect_equal(as.numeric(built$x$data[[1]]$y), c(10, 20, 30))
  expect_equal(built$x$layout$xaxis$title$text, "Week")
  expect_equal(built$x$layout$yaxis$title$text, "Cases")
  expect_equal(built$x$config$displaylogo, FALSE)
  expect_equal(built$x$config$scrollZoom, FALSE)
  expect_equal(widget$height, 400)
  heading <- as.character(htmltools::tagList(widget$prepend))
  note <- as.character(htmltools::tagList(widget$append))
  expect_match(heading, "Weekly cases", fixed = TRUE)
  expect_match(heading, "Simulated data", fixed = TRUE)
  expect_match(note, "Source: example", fixed = TRUE)
  expect_equal(p$labels$title, "Weekly cases")
})

test_that("an epidemic curve becomes interactive without recounting", {
  skip_if_not_installed("plotly")
  data <- data.frame(
    week = as.Date("2026-01-04") + 7 * 0:2,
    cases = c(0, 12, 18)
  )
  p <- islh_epi_curve(data, date = week, count = cases)
  widget <- islh_plotly(p, embed_fonts = FALSE)
  bars <- Filter(
    function(x) identical(x$type, "bar"),
    plotly::plotly_build(widget)$x$data
  )
  expect_length(bars, 1L)
  expect_equal(as.numeric(bars[[1]]$y), data$cases)
})

test_that("interactive tables have usable defaults and accept column options", {
  skip_if_not_installed("reactable")
  data <- data.frame(area = c("South", "North"), cases = c(1200, NA))
  widget <- islh_reactable(
    data,
    title = "Cases",
    subtitle = "By area",
    source_note = "Example",
    page_size = 5,
    filterable = TRUE,
    embed_fonts = FALSE,
    columns = list(cases = reactable::colDef(name = "Reported cases")),
    defaultSorted = "cases"
  )
  expect_s3_class(widget, "reactable")
  props <- widget$x$tag$attribs
  expect_equal(props$searchable, TRUE)
  expect_equal(props$filterable, TRUE)
  expect_equal(props$defaultPageSize, 5)
  expect_equal(props$columns[[2]]$name, "Reported cases")
  expect_equal(props$theme$headerStyle$background, islh_hex("blue", 20))
  expect_match(as.character(htmltools::tagList(widget$prepend)), "Cases")
  expect_match(as.character(htmltools::tagList(widget$append)), "Example")
  expect_identical(data$cases, c(1200, NA_real_))
})

test_that("table defaults can be overridden without duplicate arguments", {
  skip_if_not_installed("reactable")
  widget <- islh_reactable(
    data.frame(x = 1:2),
    searchable = FALSE,
    embed_fonts = FALSE,
    highlight = FALSE,
    defaultColDef = reactable::colDef(align = "center")
  )
  # reactable omits FALSE options from the browser payload.
  expect_null(widget$x$tag$attribs$searchable)
  expect_null(widget$x$tag$attribs$highlight)
  expect_equal(widget$x$tag$attribs$columns[[1]]$align, "center")
  expect_s3_class(
    islh_reactable(data.frame(x = numeric()), embed_fonts = FALSE),
    "reactable"
  )
})

test_that("facets and zero counts survive plot conversion", {
  skip_if_not_installed("plotly")
  data <- data.frame(
    week = rep(as.Date("2026-01-04") + 7 * 0:1, 2),
    cases = c(0, 12, 8, 20),
    area = rep(c("North", "South"), each = 2)
  )
  p <- islh_epi_curve(data, date = week, count = cases, facet = area)
  traces <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$data
  bars <- Filter(function(x) identical(x$type, "bar"), traces)
  expect_length(bars, 2L)
  expect_equal(
    sort(unlist(lapply(bars, function(x) as.numeric(x$y)))),
    sort(data$cases)
  )
})

test_that("report labels remain plain text", {
  skip_if_not_installed("reactable")
  widget <- islh_reactable(
    data.frame(label = "<script>example</script>"),
    title = htmltools::HTML("<b>Cases</b>"),
    source_note = "<script>example</script>",
    embed_fonts = FALSE
  )
  expect_match(
    as.character(htmltools::tagList(widget$prepend)),
    "&lt;b&gt;Cases&lt;/b&gt;",
    fixed = TRUE
  )
  expect_match(
    as.character(htmltools::tagList(widget$append)),
    "&lt;script&gt;example&lt;/script&gt;",
    fixed = TRUE
  )
  expect_null(widget$x$tag$attribs$columns[[1]]$html)
})

test_that("widget fonts are embedded once per dependency name", {
  skip_if_not_installed("reactable")
  local_mocked_bindings(
    .islh_bc_sans_webfont_css = function() {
      "@font-face {font-family: 'BC Sans';}"
    },
    .islh_document_has_webfont = function() FALSE
  )
  widget <- islh_reactable(data.frame(x = 1), embed_fonts = TRUE)
  expect_contains(
    vapply(widget$dependencies, `[[`, "", "name"),
    "islh-widget-font"
  )

  local_mocked_bindings(.islh_document_has_webfont = function() TRUE)
  widget <- islh_reactable(data.frame(x = 1), embed_fonts = TRUE)
  expect_equal(
    intersect(
      vapply(widget$dependencies, `[[`, "", "name"),
      "islh-widget-font"
    ),
    character()
  )
  local_mocked_bindings(
    .islh_document_has_webfont = function() FALSE,
    .islh_bc_sans_webfont_css = function() ""
  )
  widget <- islh_reactable(data.frame(x = 1), embed_fonts = TRUE)
  expect_equal(
    intersect(
      vapply(widget$dependencies, `[[`, "", "name"),
      "islh-widget-font"
    ),
    character()
  )
})

test_that("widget arguments reject repeated or unnamed options", {
  skip_if_not_installed("reactable")
  expect_snapshot(
    error = TRUE,
    islh_reactable(
      data.frame(x = 1),
      embed_fonts = FALSE,
      columns = list(),
      columns = list()
    )
  )
  expect_snapshot(
    error = TRUE,
    islh_reactable(
      data.frame(x = 1),
      NULL,
      NULL,
      NULL,
      TRUE,
      FALSE,
      10,
      FALSE,
      1
    )
  )
  expect_snapshot(
    error = TRUE,
    islh_reactable(
      data.frame(x = 1),
      embed_fonts = FALSE,
      filterable = NA
    )
  )
  expect_snapshot(error = TRUE, islh_install_deps("docx", interactive = TRUE))
})

test_that("interactive inputs produce helpful errors", {
  skip_if_not_installed("reactable")
  expect_snapshot(error = TRUE, islh_reactable(1:3, embed_fonts = FALSE))
  expect_snapshot(
    error = TRUE,
    islh_reactable(data.frame(x = 1), page_size = 0, embed_fonts = FALSE)
  )
  expect_snapshot(
    error = TRUE,
    islh_reactable(data.frame(x = 1), searchable = NA, embed_fonts = FALSE)
  )
  expect_snapshot(
    error = TRUE,
    islh_reactable(data.frame(x = 1), title = c("a", "b"), embed_fonts = FALSE)
  )
  expect_snapshot(
    error = TRUE,
    islh_reactable(data.frame(x = 1), FALSE, embed_fonts = FALSE)
  )
  skip_if_not_installed("plotly")
  expect_snapshot(error = TRUE, islh_plotly(1:3, embed_fonts = FALSE))
  expect_snapshot(
    error = TRUE,
    islh_plotly(ggplot2::ggplot(), height = -1, embed_fonts = FALSE)
  )
})

test_that("non-HTML renders explain how to use static output", {
  skip_if_not_installed("reactable")
  withr::local_options(knitr.in.progress = TRUE)
  local_mocked_bindings(
    is_html_output = function(...) FALSE,
    .package = "knitr"
  )
  expect_snapshot(
    error = TRUE,
    islh_reactable(data.frame(x = 1), embed_fonts = FALSE)
  )
})

test_that("interactive dependencies are opt-in for HTML reports", {
  basic <- islh_check("html", quiet = TRUE)
  expect_equal(intersect(basic$required, c("plotly", "reactable")), character())
  widgets <- islh_check("html", interactive = TRUE, quiet = TRUE)
  expect_contains(
    widgets$required,
    c("plotly", "reactable", "htmlwidgets", "htmltools")
  )
  expect_contains(
    islh_check(
      "html",
      tables = FALSE,
      interactive = TRUE,
      quiet = TRUE
    )$required,
    "base64enc"
  )
  expect_snapshot(
    error = TRUE,
    islh_check("docx", interactive = TRUE, quiet = TRUE)
  )
})

test_that("the installer forwards the interactive choice to every HTML check", {
  formats <- character()
  local_mocked_bindings(islh_check = function(
    format,
    interactive = FALSE,
    ...
  ) {
    if (interactive) {
      formats <<- c(formats, format)
    }
    list(
      ok = TRUE,
      format = format,
      missing = character(),
      outdated = character()
    )
  })
  expect_equal(
    islh_install_deps("both", interactive = TRUE, quiet = TRUE),
    character()
  )
  expect_equal(formats, "html")
})

test_that("interactive conversion preserves the supplied plot theme", {
  skip_if_not_installed("plotly")
  p <- ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    theme_islh(base_size = 20, grid = "none") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  expected <- plotly::plotly_build(plotly::ggplotly(p))$x$layout
  actual <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$layout
  for (axis in c("xaxis", "yaxis")) {
    expect_equal(actual[[axis]]$tickangle, expected[[axis]]$tickangle)
    expect_equal(actual[[axis]]$tickfont, expected[[axis]]$tickfont)
    expect_equal(actual[[axis]]$showgrid, expected[[axis]]$showgrid)
  }
  p <- p + theme_islh_map()
  expected <- plotly::plotly_build(plotly::ggplotly(p))$x$layout
  actual <- plotly::plotly_build(islh_plotly(p, embed_fonts = FALSE))$x$layout
  expect_equal(actual$xaxis$showticklabels, expected$xaxis$showticklabels)
  expect_equal(actual$yaxis$showticklabels, expected$yaxis$showticklabels)
})

test_that("missing and suppressed table values have explicit labels", {
  skip_if_not_installed("reactable")
  data <- data.frame(cases = c(NA_real_, 105, 17, 42))
  widget <- islh_reactable(data, embed_fonts = FALSE)
  expect_equal(widget$x$tag$attribs$columns[[1]]$na, "Missing")
  widget <- islh_reactable(
    data,
    embed_fonts = FALSE,
    columns = list(cases = reactable::colDef(na = "Suppressed"))
  )
  expect_equal(widget$x$tag$attribs$columns[[1]]$na, "Suppressed")
  expect_type(data$cases, "double")
})

test_that("interactive installation resolves the automatic output format", {
  formats <- character()
  local_mocked_bindings(
    .islh_resolve_format = function(...) "html",
    islh_check = function(format, interactive = FALSE, ...) {
      formats <<- c(formats, format)
      list(
        ok = TRUE,
        format = format,
        missing = character(),
        outdated = character()
      )
    }
  )
  expect_equal(islh_install_deps(interactive = TRUE, quiet = TRUE), character())
  expect_equal(formats, "html")

  formats <- character()
  local_mocked_bindings(.islh_resolve_format = function(...) "plots")
  expect_equal(islh_install_deps(interactive = TRUE, quiet = TRUE), character())
  expect_equal(formats, "html")

  local_mocked_bindings(.islh_resolve_format = function(...) "docx")
  expect_error(islh_install_deps(interactive = TRUE, quiet = TRUE), "requires")
})
