.islh_pal_qualitative <- function(n, reverse = FALSE) {
  if (length(n) != 1L || is.na(n) || n < 1 || n != as.integer(n)) {
    .islh_abort("{.arg n} must be one positive whole number.", call = NULL)
  }

  values <- c(
    islh_hex("blue", 50),
    islh_hex("cedar", 60),
    islh_hex("thistle", 50),
    islh_hex("fern", 50),
    islh_hex("grey", 45)
  )

  if (n > length(values)) {
    .islh_abort(
      c(
        "The Island Health qualitative palette supports at most 5 categories.",
        i = "Collapse categories, use facets, or add a non-colour cue."
      ),
      call = NULL
    )
  }

  colours <- values[seq_len(n)]

  if (isTRUE(reverse)) {
    colours <- rev(colours)
  }

  colours
}

# Ordinal scales stay within one colour family because lightness carries the
# order. Values are kept at 70 or darker for graphic contrast on white.
.islh_pal_ordinal <- function(n, family = "blue", reverse = FALSE) {
  if (length(n) != 1L || is.na(n) || n < 1 || n != as.integer(n)) {
    .islh_abort("{.arg n} must be one positive whole number.", call = NULL)
  }

  values <- switch(
    as.character(n),
    "1" = 50,
    "2" = c(25, 60),
    "3" = c(20, 45, 70),
    "4" = c(20, 40, 55, 70),
    "5" = c(15, 30, 45, 60, 70),
    .islh_abort(
      c(
        "The Island Health ordinal palette supports at most 5 categories.",
        i = "Collapse categories or use facets rather than interpolating colours."
      ),
      call = NULL
    )
  )

  colours <- islh_hex(family, values)

  if (isTRUE(reverse)) {
    colours <- rev(colours)
  }

  colours
}


# Colour for unknown, missing, or not-recorded categories.
#
# These three were top-level constants in the script. In a package that makes
# the built value depend on file collation order, so each is a function with a
# session cache instead.
.islh_unknown <- function() {
  if (is.null(.islh_state$unknown)) {
    .islh_state$unknown <- islh_hex("grey", 70)
  }
  .islh_state$unknown
}

# Sequential palette for map bins, ordered from lower to higher values. The
# five steps fall roughly 10 points of CIE lightness apart, so the ramp still
# reads in greyscale and for colour-blind readers.
.islh_pal_map <- function() {
  if (is.null(.islh_state$pal_map)) {
    .islh_state$pal_map <- islh_hex("blue", c(70, 60, 50, 40, 25))
  }
  .islh_state$pal_map
}

# Colour for areas with no data on a map.
#
# `.islh_unknown()` is grey 70, which prints at the same lightness as the
# lightest map bin. On a choropleth that makes an area with no data
# indistinguishable from the lowest band in greyscale, so maps use a much
# lighter grey and leave the darker one to categorical scales.
.islh_map_missing <- function() {
  if (is.null(.islh_state$map_missing)) {
    .islh_state$map_missing <- islh_hex("grey", 90)
  }
  .islh_state$map_missing
}

# Conventional status colours from the brand standard. Keep their meanings
# intact and pair colour with a label, icon, shape, or other non-colour cue.
.islh_pal_signal <- function() {
  if (is.null(.islh_state$pal_signal)) {
    .islh_state$pal_signal <- c(
      success = islh_brand("success"),
      warning = islh_brand("warning"),
      danger = islh_brand("danger")
    )
  }
  .islh_state$pal_signal
}
