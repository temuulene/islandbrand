# Puts the session back as it was when a test ends: theme, options, geom
# defaults, knitr, flextable and gtsummary settings, and the package's own
# state. Use it in any test that calls islh_setup().
local_clean_session <- function(env = parent.frame()) {
  state <- .islh_capture_state()
  withr::defer(.islh_restore_state(state), envir = env)
  invisible(state)
}
