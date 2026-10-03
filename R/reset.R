# Undoing islh_setup().
#
# `islh_setup()` changes the R session, not just the next plot: ggplot2's
# active theme, the discrete scale options, six geom defaults, knitr's graphics
# device, the islh.* options, flextable's defaults and gtsummary's theme. That
# is the point of it in a report, where the session exists to render one
# document. It is awkward everywhere else, because there was no way back short
# of restarting R.
#
# So `islh_setup()` now records what it found before changing anything, and
# `islh_reset()` puts it back. `with_islh()` does both around one block of code
# and takes its own record, so it nests correctly inside a session where setup
# is already active.

.islh_geom_names <- c("bar", "col", "line", "point", "smooth", "area")

.islh_option_names <- c(
  "ggplot2.discrete.colour",
  "ggplot2.discrete.fill",
  "islh.output_format",
  "islh.embed_fonts",
  "islh.document_webfont"
)

# Geom objects are exported under a capitalized name, so this reads their
# defaults through ggplot2's public interface rather than its internals, and
# keeps working across ggplot2 versions.
.islh_geom_object <- function(geom) {
  name <- paste0("Geom", toupper(substring(geom, 1, 1)), substring(geom, 2))
  tryCatch(
    getExportedValue("ggplot2", name),
    error = function(condition) NULL
  )
}

# A slot that has never been set is not the same as one set to NULL, so absence
# is recorded as NULL and presence as a one-element list.
.islh_slot <- function(name) {
  if (exists(name, envir = .islh_state, inherits = FALSE)) {
    list(value = get(name, envir = .islh_state, inherits = FALSE))
  } else {
    NULL
  }
}

.islh_restore_slot <- function(name, slot) {
  if (is.null(slot)) {
    if (exists(name, envir = .islh_state, inherits = FALSE)) {
      rm(list = name, envir = .islh_state)
    }
  } else {
    assign(name, slot$value, envir = .islh_state)
  }
  invisible(NULL)
}

#' Record everything `islh_setup()` is about to change
#'
#' @return A restore record for `.islh_restore_state()`.
#'
#' @noRd
.islh_capture_state <- function() {
  geoms <- lapply(.islh_geom_names, function(geom) {
    object <- .islh_geom_object(geom)
    if (is.null(object)) NULL else object$default_aes
  })
  names(geoms) <- .islh_geom_names

  options <- lapply(.islh_option_names, getOption)
  names(options) <- .islh_option_names

  knitr_dev <- NULL
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr_dev <- list(dev = knitr::opts_chunk$get("dev"))
  }

  flextable <- NULL
  if (requireNamespace("flextable", quietly = TRUE)) {
    flextable <- tryCatch(
      list(defaults = flextable::get_flextable_defaults()),
      error = function(condition) NULL
    )
  }

  gtsummary <- NULL
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    gtsummary <- tryCatch(
      list(theme = gtsummary::get_gtsummary_theme()),
      error = function(condition) NULL
    )
  }

  list(
    theme = ggplot2::theme_get(),
    options = options,
    geoms = geoms,
    knitr = knitr_dev,
    flextable = flextable,
    gtsummary = gtsummary,
    font = .islh_slot("font"),
    setup = .islh_slot("setup")
  )
}

#' Put back what `.islh_capture_state()` recorded
#'
#' Each step is attempted on its own. A package that has changed its interface
#' since the record was taken should leave one setting behind with a warning
#' naming it, not abandon the rest of the reset half done.
#'
#' @param state A record from `.islh_capture_state()`.
#'
#' @return `TRUE` invisibly.
#'
#' @noRd
.islh_restore_state <- function(state) {
  if (is.null(state)) {
    return(invisible(FALSE))
  }

  attempt <- function(what, expression) {
    tryCatch(
      {
        expression
        invisible(NULL)
      },
      error = function(condition) {
        .islh_warn(c(
          "Could not restore {what}.",
          x = conditionMessage(condition)
        ))
      }
    )
  }

  attempt("the ggplot2 theme", ggplot2::theme_set(state$theme))
  attempt("the plot options", options(state$options))

  for (geom in names(state$geoms)) {
    defaults <- state$geoms[[geom]]
    if (is.null(defaults)) {
      next
    }
    attempt(
      paste0("the ", geom, " geom defaults"),
      ggplot2::update_geom_defaults(geom, defaults)
    )
  }

  if (!is.null(state$knitr) && requireNamespace("knitr", quietly = TRUE)) {
    attempt(
      "the knitr graphics device",
      knitr::opts_chunk$set(dev = state$knitr$dev)
    )
  }

  if (
    !is.null(state$flextable) &&
      requireNamespace("flextable", quietly = TRUE)
  ) {
    attempt(
      "the flextable defaults",
      tryCatch(
        do.call(flextable::set_flextable_defaults, state$flextable$defaults),
        error = function(condition) flextable::init_flextable_defaults()
      )
    )
  }

  if (
    !is.null(state$gtsummary) &&
      requireNamespace("gtsummary", quietly = TRUE)
  ) {
    theme <- state$gtsummary$theme
    attempt(
      "the gtsummary theme",
      if (length(theme) == 0L) {
        suppressMessages(gtsummary::reset_gtsummary_theme())
      } else {
        suppressMessages(gtsummary::set_gtsummary_theme(theme))
      }
    )
  }

  .islh_restore_slot("font", state$font)
  .islh_restore_slot("setup", state$setup)

  invisible(TRUE)
}

#' Undo `islh_setup()`
#'
#' Puts the session back the way [islh_setup()] found it: ggplot2's theme, the
#' discrete scale options, the geom defaults, knitr's graphics device, the
#' `islh.*` options and, where they were configured, the `flextable` defaults
#' and the `gtsummary` theme.
#'
#' The record is taken by the first [islh_setup()] call of the session, so
#' calling setup several times and then resetting once returns to the state
#' before the first of them, not to the state between two of them.
#'
#' Use [with_islh()] instead when the theme is only wanted around one block of
#' code. It cannot be left applied by an error part way through.
#'
#' @param quiet Suppress the confirmation message.
#'
#' @return `TRUE` invisibly when something was restored, `FALSE` when
#'   [islh_setup()] had not run.
#' @export
#'
#' @examples
#' # Nothing to undo in a session that has not called islh_setup().
#' islh_reset(quiet = TRUE)
islh_reset <- function(quiet = FALSE) {
  quiet <- .islh_check_flag(quiet, "quiet")

  slot <- .islh_slot("setup")
  if (is.null(slot) || is.null(slot$value)) {
    if (!quiet) {
      .islh_inform(c(
        "i" = "Nothing to undo: {.fn islh_setup} has not run in this session."
      ))
    }
    return(invisible(FALSE))
  }

  # Restoring the record also clears the slot it was read from, because the
  # slot was empty when the record was taken.
  .islh_restore_state(slot$value)

  if (!quiet) {
    .islh_inform(c(
      "v" = "Island Health settings removed; the session is as
             {.fn islh_setup} found it."
    ))
  }
  invisible(TRUE)
}

#' Apply the Island Health theme around one block of code
#'
#' Runs [islh_setup()], evaluates `code`, and puts the session back afterwards
#' whether `code` succeeded or failed. Use it in a function, a test, or an
#' interactive session where the theme should not outlive the block that wanted
#' it.
#'
#' It takes its own record rather than reading the one [islh_setup()] leaves,
#' so it nests correctly: a `with_islh()` block inside a session that has
#' already run [islh_setup()] restores that setup on exit rather than removing
#' it.
#'
#' @param code Code to run with the theme applied. Its value is returned.
#' @param ... Arguments passed to [islh_setup()].
#' @param quiet Suppress the setup confirmation message. Defaults to `TRUE`,
#'   since a scoped block is usually not the place for it.
#'
#' @return The value of `code`.
#' @export
#'
#' @section Returned plots:
#'
#' ggplot2 applies the theme, the default colour scales and the geom colours
#' when a plot is drawn, not when it is built. A ggplot returned from the
#' block is therefore given its own copy of the Island Health theme, default
#' discrete colour and fill scales, and geom colours before the session is
#' restored, so it looks the same when printed afterwards. Anything the plot
#' sets itself, such as its own scale or a `theme()` addition, still wins.
#' Plots inside a plain list are handled too. Other objects are returned as
#' they are.
#'
#' @examples
#' before <- ggplot2::theme_get()
#'
#' plot <- with_islh(
#'   ggplot2::ggplot(datasets::mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point(),
#'   format = "plots"
#' )
#'
#' # The plot keeps the Island Health styling; the session does not.
#' identical(ggplot2::theme_get(), before)
with_islh <- function(code, ..., quiet = TRUE) {
  state <- .islh_capture_state()
  on.exit(.islh_restore_state(state), add = TRUE)

  islh_setup(..., quiet = quiet)
  result <- withVisible(code)

  # `on.exit()` restores the session after this line, so the plot has to take
  # the settings with it now.
  value <- .islh_freeze_plots(result$value)
  if (result$visible) value else invisible(value)
}

# ggplot2 resolves the theme, the default discrete scales and the geom
# defaults when a plot is drawn, not when it is built. A plot returned from
# `with_islh()` and printed after the block would otherwise be drawn with
# whatever the restored session has, which is usually no branding at all.
#
# Each plot, including plots inside a plain list, is given its own copy of
# what is active now. Its own `theme()` additions and scales still win.
.islh_freeze_plots <- function(value) {
  if (inherits(value, "ggplot") && !inherits(value, "patchwork")) {
    return(tryCatch(
      .islh_freeze_plot(value),
      error = function(condition) {
        .islh_warn(c(
          "Could not fix the Island Health styling to the returned plot.",
          x = conditionMessage(condition),
          i = "Draw or save the plot inside {.fn with_islh} instead."
        ))
        value
      }
    ))
  }
  if (is.list(value) && !is.object(value)) {
    return(lapply(value, .islh_freeze_plots))
  }
  value
}

.islh_freeze_plot <- function(plot) {
  # Scales first, from a build under the current settings: that is the only
  # way to learn whether colour and fill ended up discrete.
  built <- ggplot2::ggplot_build(plot)
  for (aesthetic in c("colour", "fill")) {
    scale <- built$plot$scales$get_scales(aesthetic)
    if (
      inherits(scale, "ScaleDiscrete") &&
        !plot$scales$has_scale(aesthetic)
    ) {
      plot <- plot + scale$clone()
    }
  }

  # A child of each geom that holds today's defaults as its own.
  plot$layers <- lapply(plot$layers, function(layer) {
    geom <- ggplot2::ggproto(
      NULL,
      layer$geom,
      default_aes = layer$geom$default_aes
    )
    ggplot2::ggproto(NULL, layer, geom = geom)
  })

  # A complete theme replaces the plot's theme outright, so merge the plot's
  # own additions on top of the session theme first.
  plot + (ggplot2::theme_get() + plot$theme)
}
