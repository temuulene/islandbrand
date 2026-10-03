# Shared input validation.
#
# These sit in front of the drawing, table and setup functions. A wrong size or
# flag should stop with a message naming the argument, rather than travelling
# into a figure that renders at the wrong scale or a table Word lays out one
# character wide. Each function returns its input so it can be used in place.
#
# `isTRUE()` on its own is not enough for a switch. It reads NA, 1 and "yes" as
# FALSE, so a typo silently selects the option the caller did not ask for.

.islh_check_flag <- function(x, arg, call = rlang::caller_env()) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .islh_abort("{.arg {arg}} must be a single TRUE or FALSE.", call = call)
  }
  x
}

# A physical size in inches: a figure width or height, or a page text width.
# The upper bound catches the common slip of passing a pixel count or a
# resolution where inches were wanted, which otherwise fails much later inside
# the graphics device with an unhelpful message.
.islh_check_dimension <- function(
  x,
  arg,
  maximum = 200,
  call = rlang::caller_env()
) {
  if (
    !is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0
  ) {
    .islh_abort(
      "{.arg {arg}} must be one positive, finite number of inches.",
      call = call
    )
  }
  if (x > maximum) {
    .islh_abort(
      c(
        "{.arg {arg}} is {x} inches, which is larger than any page.",
        i = "Sizes are given in inches, not points or pixels."
      ),
      call = call
    )
  }
  as.numeric(x)
}

# Output resolution in dots per inch. 300 suits a report and 192 a slide; the
# ceiling is well past any print requirement and stops a typo producing a file
# too large to open.
.islh_check_dpi <- function(
  x,
  arg = "dpi",
  maximum = 2400,
  call = rlang::caller_env()
) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1) {
    .islh_abort(
      "{.arg {arg}} must be one positive, finite resolution in dots per inch.",
      call = call
    )
  }
  if (x > maximum) {
    .islh_abort(
      c(
        "{.arg {arg}} is {x} dots per inch, far beyond what printing needs.",
        i = "Use 300 for a report or 192 for a slide."
      ),
      call = call
    )
  }
  as.numeric(x)
}

# A width given as a share of the available text width, so a table can be told
# to fill the page or six tenths of it.
.islh_check_fraction <- function(x, arg, call = rlang::caller_env()) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0 ||
      x > 1
  ) {
    .islh_abort(
      c(
        "{.arg {arg}} must be one number greater than 0 and at most 1.",
        i = "It is a share of the text width, so {.code 0.6} is a table filling
           six tenths of the page."
      ),
      call = call
    )
  }
  as.numeric(x)
}

# A base font size in points.
.islh_check_size <- function(
  x,
  arg = "base_size",
  maximum = 100,
  call = rlang::caller_env()
) {
  if (
    !is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0
  ) {
    .islh_abort(
      "{.arg {arg}} must be one positive, finite font size in points.",
      call = call
    )
  }
  if (x > maximum) {
    .islh_abort(
      c(
        "{.arg {arg}} is {x} points, which no figure can show.",
        i = "Font sizes are given in points; 12 is the Island Health default."
      ),
      call = call
    )
  }
  as.numeric(x)
}

# A whole positive count, such as a limit on how many shapes to draw.
.islh_check_count <- function(x, arg, call = rlang::caller_env()) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x != round(x) ||
      x < 1
  ) {
    .islh_abort("{.arg {arg}} must be one positive whole number.", call = call)
  }
  as.integer(x)
}

# A position inside a panel, as fractions from the bottom left corner.
.islh_check_position <- function(x, arg, call = rlang::caller_env()) {
  if (
    !is.numeric(x) ||
      length(x) != 2L ||
      anyNA(x) ||
      any(!is.finite(x)) ||
      any(x < 0) ||
      any(x > 1)
  ) {
    .islh_abort(
      c(
        "{.arg {arg}} must be two fractions between 0 and 1.",
        i = "They are fractions of the panel measured from its bottom left
           corner."
      ),
      call = call
    )
  }
  as.numeric(x)
}

# An optional column named by a string, such as a table's group or row-label
# column. A typo should name the argument, not surface later as an unlabelled
# table.
.islh_check_column <- function(x, data, arg, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    .islh_abort(
      "{.arg {arg}} must be one column name, as a string.",
      call = call
    )
  }
  if (!x %in% names(data)) {
    .islh_abort(
      c(
        "{.arg {arg}} names {.field {x}}, which is not a column in the data.",
        i = "Columns: {.field {names(data)}}."
      ),
      call = call
    )
  }
  x
}
