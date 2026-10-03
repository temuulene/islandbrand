# Mutable package state.
#
# A package namespace is locked once loaded, so `<<-` fails at runtime.
# Anything the package needs to remember between calls lives in this
# environment instead. `islh_font_family()` and the webfont builder both cache
# here. Creating the environment inspects nothing and changes nothing, so
# loading the package stays inert.
.islh_state <- new.env(parent = emptyenv())

# Official default on Island Health-managed laptops. `islh_setup()` confirms
# which family is actually installed and `islh_font_family()` caches the answer.
.islh_default_font <- "BC Sans"

# The brand font resolved by the last `islh_font_family()` call, or the Island
# Health default when it has not run yet. Nothing here probes the system: the
# helpers that draw plots and tables need a family name whether or not the user
# has called `islh_setup()`.
.islh_font <- function() {
  if (!exists("font", envir = .islh_state, inherits = FALSE)) {
    return(.islh_default_font)
  }
  font <- .islh_state$font
  if (is.null(font)) .islh_default_font else font
}
