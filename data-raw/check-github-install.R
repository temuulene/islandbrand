# Exercise staff installation with a clean library and no compiler tools.
# An optional second argument selects the candidate checkout instead of the
# public repository, so pull requests can test their changes before a merge.
check_github_install <- function(repo, candidate = NULL) {
  if (.Platform$OS.type != "windows") {
    stop("Run this check on Windows.")
  }
  library <- tempfile("staff-library-")
  dir.create(library)
  .libPaths(c(library, .Library), include.site = FALSE)
  Sys.setenv(R_LIBS_USER = library, R_LIBS_SITE = "")
  # Keep only Windows and R on PATH. A preinstalled compiler must not make a
  # source dependency appear to work on a staff machine without build tools.
  Sys.setenv(
    PATH = paste(
      c(
        R.home("bin"),
        Sys.getenv("SystemRoot"),
        file.path(Sys.getenv("SystemRoot"), "System32")
      ),
      collapse = ";"
    )
  )
  Sys.unsetenv(c(
    "RTOOLS_HOME",
    paste0("RTOOLS", c("40", "42", "43", "44", "45", "46"), "_HOME"),
    "BINPREF"
  ))
  tools <- c("make", "gcc", "g++", "gfortran")
  stopifnot(all(Sys.which(tools) == ""))
  # Leave R's default package type alone: the README's `type = "binary"`
  # must be what keeps every dependency a prebuilt binary.
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  utils::install.packages(
    "remotes",
    lib = library,
    type = "binary",
    quiet = TRUE
  )
  if (!is.null(candidate)) {
    # Also exercise a real, outdated staff dependency. ggplot2 is pure R,
    # so its archived source can be installed with compiler tools absent.
    # Install its dependency stack as binaries before adding the old version;
    # install_version() otherwise asks for source dependencies too.
    utils::install.packages(
      c("ggplot2", "tibble"),
      lib = library,
      type = "binary",
      quiet = TRUE
    )
    remotes::install_version(
      "ggplot2",
      version = "3.4.4",
      lib = library,
      dependencies = FALSE,
      upgrade = "never",
      build = FALSE,
      quiet = TRUE
    )
    stopifnot(utils::packageVersion("ggplot2") == "3.4.4")
    message("Starting with ggplot2 below the required minimum.")
  }
  if (is.null(candidate)) {
    # The README command, plus a clean library and no token.
    remotes::install_github(
      repo,
      type = "binary",
      upgrade = "always",
      build = FALSE,
      lib = library,
      auth_token = NULL,
      quiet = TRUE
    )
    message("Public GitHub installation completed without compiler tools.")
  } else {
    remotes::install_local(
      candidate,
      type = "binary",
      upgrade = "always",
      build = FALSE,
      lib = library,
      force = TRUE,
      quiet = TRUE
    )
    message("Candidate checkout installed by the same source-install route.")
  }
  stopifnot(requireNamespace("islandbrand", quietly = TRUE))
  stopifnot(utils::packageVersion("ggplot2") >= "3.5.0")
  counts <- data.frame(
    week = as.Date("2026-01-04") + 7 * 0:1,
    cases = c(10, 15)
  )
  plot <- islandbrand::islh_epi_curve(counts, week, cases)
  stopifnot(inherits(plot, "ggplot"))
  if (
    !all(
      c("islh_plotly", "islh_reactable") %in%
        getNamespaceExports("islandbrand")
    )
  ) {
    if (!is.null(candidate)) {
      stop("The candidate is missing interactive helpers.")
    }
    message("Public package installed and its core plotting helper is ready.")
    return(invisible(library))
  }
  islandbrand::islh_install_deps("html", interactive = TRUE, quiet = TRUE)
  stopifnot(
    islandbrand::islh_check("html", interactive = TRUE, quiet = TRUE)$ok
  )
  widget <- islandbrand::islh_plotly(plot, embed_fonts = FALSE)
  table <- islandbrand::islh_reactable(counts, embed_fonts = FALSE)
  stopifnot(
    inherits(widget, "plotly"),
    inherits(table, "reactable"),
    all(Sys.which(tools) == "")
  )
  message("Interactive plots and tables are ready; no Rtools was needed.")
  invisible(library)
}

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1]] else "temuulene/islandbrand"
candidate <- if (length(args) > 1L) normalizePath(args[[2]], mustWork = TRUE)
check_github_install(repo, candidate)
