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
  options(
    repos = c(CRAN = "https://cloud.r-project.org"),
    pkgType = "binary",
    install.packages.check.source = "no"
  )
  utils::install.packages(
    "remotes",
    lib = library,
    type = "binary",
    quiet = TRUE
  )
  if (is.null(candidate)) {
    remotes::install_github(
      repo,
      lib = library,
      dependencies = NA,
      auth_token = NULL,
      upgrade = "never",
      build = FALSE,
      build_vignettes = FALSE,
      quiet = TRUE
    )
    message("Public GitHub installation completed without compiler tools.")
  } else {
    remotes::install_local(
      candidate,
      lib = library,
      dependencies = NA,
      upgrade = "never",
      build = FALSE,
      build_vignettes = FALSE,
      force = TRUE,
      quiet = TRUE
    )
    message("Candidate checkout installed by the same source-install route.")
  }
  stopifnot(requireNamespace("islandbrand", quietly = TRUE))
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
