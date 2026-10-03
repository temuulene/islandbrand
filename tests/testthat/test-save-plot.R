test_that("islh_save_plot checks its arguments", {
  skip_if_not_installed("ragg")
  plot <- ggplot2::ggplot()

  expect_error(islh_save_plot(c("a.png", "b.png"), plot), "one file path")
  expect_error(islh_save_plot(NA_character_, plot), "one file path")
  expect_error(islh_save_plot("figure.jpg", plot), ".png")
  expect_error(islh_save_plot("figure.png", plot, width = 650), "inches")
  expect_error(islh_save_plot("figure.png", plot, dpi = 0), "dots per inch")
})

test_that("islh_save_plot writes a PNG at the preset size", {
  skip_if_not_installed("ragg")
  path <- withr::local_tempfile(fileext = ".png")
  plot <- ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  expect_equal(islh_save_plot(path, plot, preset = "slide"), path)
  # 10 x 5.625 inches at 192 dots per inch. A PNG stores its width and height
  # as big-endian integers in bytes 17 to 24.
  header <- readBin(path, "raw", n = 24L)
  size <- readBin(header[17:24], "integer", n = 2L, size = 4L, endian = "big")
  expect_equal(size, c(1920L, 1080L))
})
