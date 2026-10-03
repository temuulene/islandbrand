# Loading must stay inert: no font probing, no options, no messages, no
# `theme_set()`. All of that belongs in `islh_setup()`, which the user calls
# when they are ready.
#
# In particular this must not seed `.islh_state$font`. `islh_font_family()`
# treats the slot's existence as "already resolved", so creating it here would
# make the package report a font it never looked for.
.onLoad <- function(libname, pkgname) {
  invisible(NULL)
}
