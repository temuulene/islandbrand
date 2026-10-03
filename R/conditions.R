# Conditions
#
# Thin wrappers so every message in the package carries consistent formatting
# and, for errors, names the user-facing function rather than the internal
# helper that raised it.
#
# `call` does that naming. Its default is the frame that called
# `.islh_abort()`, which is right for an exported function and wrong for a
# shared helper such as `.islh_check_flag()`: the user never called that. So
# every internal helper that can abort takes `call = rlang::caller_env()` and
# passes it on, and the error reports the exported function that was called.
#
# `.envir` is the load-bearing argument. cli interpolates `{...}` in `.envir`,
# which defaults to the calling frame — and the calling frame of
# `cli::cli_abort()` here is this wrapper, where a caller's local variables do
# not exist. Without forwarding it, `{package}` in a caller's message resolves
# to nothing (or worse, to an unrelated base function such as `dir`), and the
# message fails to build. `parent.frame()` in the default argument is evaluated
# lazily inside the wrapper, so it resolves to whoever called it.

.islh_abort <- function(
  message,
  call = parent.frame(),
  .envir = parent.frame(),
  parent = NULL
) {
  cli::cli_abort(
    message,
    call = call,
    .envir = .envir,
    parent = parent,
    class = "islh_error"
  )
}

.islh_warn <- function(message, .envir = parent.frame()) {
  cli::cli_warn(message, .envir = .envir)
  invisible(NULL)
}

.islh_inform <- function(message, .envir = parent.frame()) {
  cli::cli_inform(message, .envir = .envir)
  invisible(NULL)
}

# Builds the install command shown in error messages. Island Health laptops
# block programs run from a user library, which breaks `pak`, and have no
# compiler, so a source build fails too. Binary installs through base R's
# `install.packages()` are the only route that works. Braces are escaped
# because the result is interpolated into a cli `{.code ...}` span.
.islh_install_command <- function(packages) {
  packages <- unique(packages)
  quoted <- paste(sprintf('"%s"', packages), collapse = ", ")
  command <- paste0('install.packages(c(', quoted, '), type = "binary")')
  gsub("}", "}}", gsub("{", "{{", command, fixed = TRUE), fixed = TRUE)
}

.islh_require <- function(package, feature, call = rlang::caller_env()) {
  if (!requireNamespace(package, quietly = TRUE)) {
    .islh_abort(
      c(
        "Package {.pkg {package}} is required for {feature}.",
        i = paste0(
          "Install it with {.code ",
          .islh_install_command(package),
          "} and try again."
        )
      ),
      call = call
    )
  }
  invisible(TRUE)
}
