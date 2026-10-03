# islh_setup() changes the session, so every test here has to put it back
# itself or it corrupts the ones that follow.
local_clean_session <- function(env = parent.frame()) {
  state <- .islh_capture_state()
  withr::defer(.islh_restore_state(state), envir = env)
  invisible(state)
}

test_that("setup records a way back and reset takes it", {
  local_clean_session()

  before_theme <- ggplot2::theme_get()
  before_fill <- ggplot2::GeomBar$default_aes$fill
  before_discrete <- getOption("ggplot2.discrete.fill")

  suppressWarnings(islh_setup(format = "plots", quiet = TRUE))

  expect_false(identical(ggplot2::theme_get(), before_theme))
  expect_false(identical(ggplot2::GeomBar$default_aes$fill, before_fill))
  expect_false(is.null(getOption("ggplot2.discrete.fill")))
  expect_equal(getOption("islh.output_format"), "plots")

  expect_true(islh_reset(quiet = TRUE))

  expect_identical(ggplot2::theme_get(), before_theme)
  expect_identical(ggplot2::GeomBar$default_aes$fill, before_fill)
  expect_identical(getOption("ggplot2.discrete.fill"), before_discrete)
  expect_null(getOption("islh.output_format"))
})

test_that("reset restores the knitr device it found", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("ragg")
  local_clean_session()

  before <- knitr::opts_chunk$get("dev")
  suppressWarnings(islh_setup(format = "plots", quiet = TRUE))
  expect_equal(knitr::opts_chunk$get("dev"), "ragg_png")

  islh_reset(quiet = TRUE)
  expect_identical(knitr::opts_chunk$get("dev"), before)
})

test_that("reset on an untouched session reports that there is nothing to do", {
  local_clean_session()
  if (exists("setup", envir = .islh_state, inherits = FALSE)) {
    rm("setup", envir = .islh_state)
  }

  expect_false(islh_reset(quiet = TRUE))
  expect_message(islh_reset(), "has not run")
})

test_that("repeated setup keeps the first way back", {
  local_clean_session()

  before <- ggplot2::theme_get()
  suppressWarnings(islh_setup(format = "plots", base_size = 12, quiet = TRUE))
  first_record <- .islh_state$setup

  suppressWarnings(islh_setup(format = "plots", base_size = 18, quiet = TRUE))

  # The second call must not record the first call's settings as the way back,
  # or reset would leave the Island Health theme applied.
  expect_identical(.islh_state$setup, first_record)

  islh_reset(quiet = TRUE)
  expect_identical(ggplot2::theme_get(), before)
})

test_that("with_islh applies the theme only inside the block", {
  local_clean_session()

  before <- ggplot2::theme_get()
  inside <- suppressWarnings(
    with_islh(ggplot2::theme_get(), format = "plots")
  )

  expect_false(identical(inside, before))
  expect_identical(ggplot2::theme_get(), before)
})

test_that("a plot returned from with_islh keeps its styling", {
  local_clean_session()

  # ggplot2 applies the theme, default scales and geom colours when a plot is
  # drawn. Without freezing, a plot printed after the block is unbranded.
  plots <- suppressWarnings(with_islh(
    list(
      mapped = ggplot2::ggplot(
        datasets::mtcars,
        ggplot2::aes(factor(cyl), fill = factor(gear))
      ) +
        ggplot2::geom_bar() +
        ggplot2::theme(legend.position = "top"),
      unmapped = ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt)) +
        ggplot2::geom_histogram(bins = 5),
      continuous = ggplot2::ggplot(
        datasets::mtcars,
        ggplot2::aes(wt, mpg, colour = hp)
      ) +
        ggplot2::geom_point()
    ),
    format = "plots"
  ))

  mapped <- ggplot2::ggplot_build(plots$mapped)$data[[1]]
  expect_setequal(unique(mapped$fill), .islh_pal_qualitative(3))

  unmapped <- ggplot2::ggplot_build(plots$unmapped)$data[[1]]
  expect_equal(unique(unmapped$fill), islh_brand("primary"))

  # A continuous mapping is left to its own scale, not forced to discrete.
  expect_no_error(ggplot2::ggplot_build(plots$continuous))

  # The plot's own theme addition wins over the frozen session theme.
  expect_equal(ggplot2:::plot_theme(plots$mapped)$legend.position, "top")
  expect_equal(
    ggplot2:::plot_theme(plots$mapped)$plot.title$colour,
    islh_hex("blue", 20)
  )
})

test_that("a plot's own colour scale is not replaced", {
  local_clean_session()

  plot <- suppressWarnings(with_islh(
    ggplot2::ggplot(
      datasets::mtcars,
      ggplot2::aes(factor(cyl), fill = factor(am))
    ) +
      ggplot2::geom_bar() +
      ggplot2::scale_fill_manual(values = c("black", "orange")),
    format = "plots"
  ))

  fills <- unique(ggplot2::ggplot_build(plot)$data[[1]]$fill)
  expect_setequal(fills, c("black", "orange"))
})

test_that("with_islh keeps the visibility of its value", {
  local_clean_session()
  expect_invisible(suppressWarnings(with_islh(invisible(1), format = "plots")))
  expect_visible(suppressWarnings(with_islh(1, format = "plots")))
})

test_that("with_islh restores the session when the block fails", {
  local_clean_session()

  before <- ggplot2::theme_get()
  expect_error(
    suppressWarnings(with_islh(stop("something went wrong"), format = "plots")),
    "something went wrong"
  )
  expect_identical(ggplot2::theme_get(), before)
})

test_that("with_islh returns the value of its code", {
  local_clean_session()
  expect_equal(suppressWarnings(with_islh(6 * 7, format = "plots")), 42)
})

test_that("with_islh nests inside an active setup without undoing it", {
  local_clean_session()

  suppressWarnings(islh_setup(format = "plots", quiet = TRUE))
  active <- ggplot2::theme_get()
  record <- .islh_state$setup

  suppressWarnings(with_islh(invisible(NULL), format = "plots"))

  # Leaving the block must return to the surrounding setup, not remove it.
  expect_identical(ggplot2::theme_get(), active)
  expect_identical(.islh_state$setup, record)

  islh_reset(quiet = TRUE)
  expect_false(exists("setup", envir = .islh_state, inherits = FALSE))
})

test_that("reset arguments are checked", {
  expect_error(islh_reset(quiet = NA), "single TRUE or FALSE")
})
