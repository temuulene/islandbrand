test_that("the example LHA map holds all 14 areas with 2025 population", {
  skip_if_not_installed("sf")
  lha <- islh_example_lha()

  expect_s3_class(lha, "sf")
  expect_equal(nrow(lha), 14L)
  expect_equal(lha$geography_code, islh_areas()$code)
  expect_equal(lha$geography_name, islh_areas()$name)
  expect_equal(lha$hsda, islh_areas()$hsda)

  # BC Stats 2025 estimates, all sexes, as built by data-raw.
  expect_equal(unique(lha$year), 2025L)
  expect_equal(sum(lha$population), 927705)
  expect_equal(
    lha$population[lha$geography_name == "Greater Victoria"],
    258999
  )
})

test_that("the example LHA map is valid and in BC Albers", {
  skip_if_not_installed("sf")
  lha <- islh_example_lha()

  expect_equal(sf::st_crs(lha)$epsg, 3005L)
  expect_true(all(sf::st_is_valid(lha)))
  expect_equal(attr(lha, "sf_column"), "geometry")
})

test_that("the example files are installed", {
  expect_true(file.exists(islh_logo()))
  expect_true(file.exists(islh_brand_yml()))
  expect_true(file.exists(islh_reference_docx()))
  expect_equal(nrow(islh_example_data()), 3L)
})

test_that("the example LHA map ships with its build record", {
  path <- system.file(
    "extdata",
    "islh-lha-provenance.csv",
    package = "islandbrand"
  )
  expect_true(nzchar(path))
  record <- utils::read.csv(path, colClasses = "character")
  value <- stats::setNames(record$value, record$field)

  for (field in c(
    "retrieved_utc",
    "boundary_url",
    "population_url",
    "population_year",
    "crs",
    "simplification"
  )) {
    expect_true(nzchar(value[[field]]) && !is.na(value[[field]]), label = field)
  }
  expect_match(value[["retrieved_utc"]], "^\\d{4}-\\d{2}-\\d{2}T")
  expect_equal(value[["crs"]], "EPSG:3005")

  skip_if_not_installed("sf")
  expect_equal(
    as.character(unique(islh_example_lha()$year)),
    value[["population_year"]]
  )
})
