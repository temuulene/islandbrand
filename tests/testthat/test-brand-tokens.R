# Brand hexes exist in three places: `.islh_colours` (the source), the
# generated `tokens.json` the Word reference-doc builder reads, and
# `_brand.yml` that Quarto and Shiny read. These tests fail if they drift.

test_that("tokens.json matches the colour table it is generated from", {
  skip_if_not_installed("yaml")
  path <- system.file("brand", "tokens.json", package = "islandbrand")
  skip_if(!nzchar(path), "tokens.json is not installed")

  # yaml is a superset of JSON, so it parses tokens.json without adding a
  # second parser dependency.
  tokens <- yaml::read_yaml(path)

  expect_setequal(names(tokens$families), names(.islh_colours))

  for (family in names(.islh_colours)) {
    entry <- .islh_colours[[family]]
    published <- tokens$families[[family]]
    expect_setequal(names(published), names(entry))
    expect_equal(
      toupper(unlist(published[names(entry)])),
      toupper(unname(entry)),
      ignore_attr = TRUE,
      label = paste0("tokens.json family '", family, "'")
    )
  }

  expect_equal(
    toupper(unlist(tokens$named[names(.islh_brand_colours)])),
    toupper(unname(.islh_brand_colours)),
    ignore_attr = TRUE
  )
})

test_that("_brand.yml matches the named brand colours", {
  skip_if_not_installed("yaml")
  brand <- yaml::read_yaml(islh_brand_yml())

  # `_brand.yml` uses the brand's own swatch names; islh_brand() uses role
  # names. This mapping is the contract between the two files.
  mapping <- c(
    blue = "primary",
    "blue-dark" = "primary_dark",
    "blue-light" = "primary_light",
    grey = "secondary",
    red = "danger",
    green = "success",
    orange = "warning",
    thistle = "thistle",
    fern = "fern",
    cedar = "cedar",
    white = "white",
    black = "black"
  )

  palette <- brand$color$palette
  expect_setequal(names(palette), names(mapping))

  for (swatch in names(mapping)) {
    expect_equal(
      toupper(palette[[swatch]]),
      toupper(unname(islh_brand(mapping[[swatch]]))),
      label = paste0("_brand.yml colour '", swatch, "'")
    )
  }
})

test_that("_brand.yml roles resolve to palette entries", {
  skip_if_not_installed("yaml")
  brand <- yaml::read_yaml(islh_brand_yml())
  roles <- brand$color[setdiff(names(brand$color), "palette")]

  # Every role must name a swatch that exists, or Quarto silently drops it.
  expect_true(all(unlist(roles) %in% names(brand$color$palette)))
})

test_that("every logo the accessor can name is installed", {
  grid <- expand.grid(
    lockup = c("horizontal", "stacked"),
    variant = c("full-colour", "dark-blue", "white", "black"),
    format = c("svg", "png"),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(grid))) {
    path <- islh_logo(grid$lockup[i], grid$variant[i], grid$format[i])
    expect_true(
      file.exists(path),
      label = paste(grid$lockup[i], grid$variant[i], grid$format[i])
    )
  }
})

test_that("the example data has the columns the scaffold expects", {
  data <- islh_example_data()
  expect_s3_class(data, "data.frame")
  expect_named(data, c("program", "encounters", "median_wait_minutes"))
  expect_gt(nrow(data), 0L)
})
