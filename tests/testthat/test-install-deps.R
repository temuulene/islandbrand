# islh_install_deps() never really installs here: .islh_install_packages() is
# replaced so the test sees what would have been installed, and how.

local_fake_installer <- function(env = parent.frame()) {
  calls <- new.env()
  calls$packages <- NULL
  calls$type <- NULL
  local_mocked_bindings(
    .islh_install_packages = function(packages, type) {
      calls$packages <- packages
      calls$type <- type
      invisible(NULL)
    },
    .env = env
  )
  calls
}

test_that("only what the check reports as missing is installed", {
  calls <- local_fake_installer()
  local_mocked_bindings(
    islh_check = function(format, ...) {
      structure(
        list(
          ok = FALSE,
          format = format,
          tables = TRUE,
          embed_fonts = FALSE,
          required = c("ggplot2", "gt"),
          missing = "gt",
          outdated = character(),
          install_command = ""
        ),
        class = "islh_dependency_check"
      )
    }
  )

  # The fake installer installs nothing, so the check afterwards still fails.
  expect_warning(
    result <- islh_install_deps("html", quiet = TRUE),
    "still not ready"
  )
  expect_equal(result, "gt")
  expect_equal(calls$packages, "gt")
  if (.Platform$OS.type == "windows") {
    expect_equal(calls$type, "binary")
  }
})

test_that("nothing is installed when the format is ready", {
  calls <- local_fake_installer()
  local_mocked_bindings(
    islh_check = function(format, ...) {
      structure(
        list(
          ok = TRUE,
          format = format,
          missing = character(),
          outdated = character()
        ),
        class = "islh_dependency_check"
      )
    }
  )

  expect_equal(islh_install_deps("plots", quiet = TRUE), character())
  expect_null(calls$packages)
})

test_that("upgrading a loaded package asks for a restart", {
  calls <- local_fake_installer()
  local_mocked_bindings(
    islh_check = function(format, ...) {
      structure(
        list(
          ok = FALSE,
          format = format,
          tables = FALSE,
          embed_fonts = FALSE,
          required = "ggplot2",
          missing = character(),
          outdated = "ggplot2",
          install_command = ""
        ),
        class = "islh_dependency_check"
      )
    }
  )

  messages <- testthat::capture_messages(
    suppressWarnings(islh_install_deps("plots"))
  )
  expect_true(any(grepl("Restart R", messages, fixed = TRUE)))
})

test_that("the installer is base R's, never pak", {
  body <- paste(deparse(.islh_install_packages), collapse = "\n")
  expect_match(body, "utils::install.packages", fixed = TRUE)
  expect_false(grepl("pak", body, fixed = TRUE))
})

test_that("a quiet install still warns when packages are not ready", {
  calls <- local_fake_installer()
  local_mocked_bindings(
    islh_check = function(format, ...) {
      structure(
        list(
          ok = FALSE,
          format = format,
          tables = TRUE,
          embed_fonts = FALSE,
          required = "gt",
          missing = "gt",
          outdated = character(),
          install_command = ""
        ),
        class = "islh_dependency_check"
      )
    }
  )

  expect_warning(
    expect_no_message(result <- islh_install_deps("html", quiet = TRUE)),
    "still not ready"
  )
  expect_equal(result, "gt")
  expect_error(islh_install_deps("html", quiet = NA), "TRUE or FALSE")
})

test_that("the README installs every import before the package file", {
  # R loads the imports before any islandbrand function can run, so the
  # README's first command must cover all of them. Source checkouts only:
  # README.md is not part of the installed package.
  readme <- test_path("..", "..", "README.md")
  description <- test_path("..", "..", "DESCRIPTION")
  skip_if_not(file.exists(readme) && file.exists(description))

  imports <- read.dcf(description, fields = "Imports")[1, 1]
  imports <- trimws(sub("\\(.*", "", strsplit(imports, ",")[[1]]))
  base <- rownames(utils::installed.packages(priority = "base"))
  imports <- sort(setdiff(imports, base))

  text <- paste(readLines(readme), collapse = "\n")
  command <- regmatches(
    text,
    regexpr("install\\.packages\\(\\s*c\\([^)]*\\)", text)
  )
  listed <- sort(regmatches(
    command,
    gregexpr('(?<=")[^",]+(?=")', command, perl = TRUE)
  )[[1]])
  expect_equal(listed, imports)
})
