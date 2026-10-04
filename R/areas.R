# Colours for Island Health's health geographies.
#
# Staff often compare the 14 local health areas (LHAs) in one figure. No
# palette makes 14 categories identifiable by colour alone, and the brand
# standard forbids relying on colour alone when it carries meaning. So colour
# is organised in two levels:
#
# * The health service delivery area (HSDA) picks the colour family. South,
#   Central and North Vancouver Island take Blue, Cedar and Thistle: low-signal
#   families, which the brand standard reserves for organising content, with
#   Blue, the primary family, among them. Red, Green and Orange keep their
#   signal meanings, and Grey keeps "inactive or unavailable".
# * The LHA picks the value within the family.
#
# Values were chosen against LHA adjacency computed from the BC Data Catalogue
# boundaries. Neighbours within an HSDA differ by at least 20 values, and
# neighbours across HSDAs by at least 10 as well as in hue. South and North
# never touch, which keeps Blue and Thistle, the pair protanopia confuses most,
# apart on every map.
#
# Contrast is measured, not inferred from the values. Brand value differences
# do not translate exactly into WCAG ratios: Blue 50 on white is 4.45:1, just
# short of the 4.5:1 that text needs.
#
# * Labels: islh_areas() picks white or Grey 10, whichever contrasts more with
#   the fill. Every pair reaches 4.5:1, enough for text of any size. South
#   Vancouver Island is Blue 45 rather than Blue 50 for this reason.
# * Lines: the lightest fills (value 70) are about 2.3:1 on white, below the
#   3:1 a line needs to be seen. Lines use a colour 10 values darker than the
#   fill, which keeps the steps between areas and reaches 3:1 for all of them.
# * Fills: on a map, areas meet each other more than the page, are outlined,
#   and are labelled, so colour against white is not the only cue.
#
# Codes and names are the BC Data Catalogue's Local Health Area boundaries.

.islh_hsda_definition <- data.frame(
  code = c("41", "42", "43"),
  name = c(
    "South Vancouver Island",
    "Central Vancouver Island",
    "North Vancouver Island"
  ),
  family = c("blue", "cedar", "thistle"),
  value = c(45, 60, 50),
  stringsAsFactors = FALSE
)

.islh_lha_definition <- data.frame(
  code = c(
    "411",
    "412",
    "413",
    "414",
    "421",
    "422",
    "423",
    "424",
    "425",
    "426",
    "431",
    "432",
    "433",
    "434"
  ),
  name = c(
    "Greater Victoria",
    "Western Communities",
    "Saanich Peninsula",
    "Southern Gulf Islands",
    "Cowichan Valley South",
    "Cowichan Valley West",
    "Cowichan Valley North",
    "Greater Nanaimo",
    "Oceanside",
    "Alberni/Clayoquot",
    "Comox Valley",
    "Greater Campbell River",
    "Vancouver Island West",
    "Vancouver Island North"
  ),
  value = c(20, 45, 70, 30, 30, 70, 50, 20, 60, 40, 30, 70, 20, 45),
  stringsAsFactors = FALSE
)

# Other spellings in common use, matched as well as the official names.
.islh_area_aliases <- c(
  "Alberni-Clayoquot" = "Alberni/Clayoquot",
  "Alberni Clayoquot" = "Alberni/Clayoquot"
)

#' Island Health health service delivery areas and local health areas
#'
#' Returns the brand colour for each of Island Health's 3 health service
#' delivery areas (HSDAs) or 14 local health areas (LHAs), with the codes and
#' names used by the BC Data Catalogue.
#'
#' Each HSDA has its own colour family: Blue for South, Cedar for Central and
#' Thistle for North Vancouver Island. Each LHA takes a different value within
#' its HSDA's family, so a figure reads by HSDA at a glance and neighbouring
#' LHAs stay distinct on a map. The colours are fixed, so an LHA looks the same
#' in every report.
#'
#' The brand standard allows more than two colour families only when colour
#' carries meaning, and then never as the only cue. Label each area, on the
#' map or at the end of each line. For line charts, facetting by `hsda` leaves
#' 4 to 6 lines of one family in each panel.
#'
#' @param level `"lha"` for local health areas or `"hsda"` for health service
#'   delivery areas.
#'
#' @return A data frame in code order with columns `code`, `name`, `colour`,
#'   `label_colour` and `line_colour`, plus `hsda` for LHAs.
#'
#'   * `colour` is the fill for a map or a bar.
#'   * `label_colour` is white or Grey 10, whichever contrasts more with
#'     `colour`. Every pair reaches at least 4.5:1, the WCAG minimum for text
#'     of any size, so a label can sit inside the shape.
#'   * `line_colour` is `colour` 10 values darker. Every one reaches at least
#'     3:1 against a white page, the WCAG minimum for a line a reader needs
#'     to see. [scale_colour_islh_area()] uses it.
#'
#' @seealso [scale_fill_islh_area()] to use the colours in a plot.
#'
#' @examples
#' islh_areas()
#' islh_areas("hsda")
#'
#' # A named vector, for scale_fill_manual() or another package:
#' areas <- islh_areas()
#' stats::setNames(areas$colour, areas$name)
#'
#' @export
islh_areas <- function(level = c("lha", "hsda")) {
  level <- match.arg(level)
  cache <- paste0("areas_", level)
  if (is.null(.islh_state[[cache]])) {
    .islh_state[[cache]] <- .islh_build_areas(level)
  }
  .islh_state[[cache]]
}

.islh_build_areas <- function(level) {
  hsda <- .islh_hsda_definition
  if (level == "hsda") {
    areas <- hsda
  } else {
    areas <- .islh_lha_definition
    parent <- match(substr(areas$code, 1L, 2L), hsda$code)
    areas$hsda <- hsda$name[parent]
    areas$family <- hsda$family[parent]
  }

  areas$colour <- mapply(islh_hex, areas$family, areas$value, USE.NAMES = FALSE)
  # Whichever of white and Grey 10 contrasts more with the fill, measured.
  white <- islh_brand("white")
  dark <- islh_hex("grey", 10)
  areas$label_colour <- ifelse(
    .islh_contrast_ratio(white, areas$colour) >=
      .islh_contrast_ratio(dark, areas$colour),
    white,
    dark
  )
  areas$line_colour <- mapply(
    islh_hex,
    areas$family,
    areas$value - 10,
    USE.NAMES = FALSE
  )

  columns <- c(
    "code",
    "name",
    if (level == "lha") "hsda",
    "colour",
    "label_colour",
    "line_colour"
  )
  out <- areas[columns]
  rownames(out) <- NULL
  out
}

# Every key a scale matches, name or code or alias, to its colour, with the
# official names first so the legend lists them in code order. `column` is
# "colour" for fills and "line_colour" for lines and points.
.islh_area_values <- function(level, column = "colour") {
  areas <- islh_areas(level)
  by_name <- stats::setNames(areas[[column]], areas$name)
  by_code <- stats::setNames(areas[[column]], areas$code)
  values <- c(by_name, by_code)
  if (level == "lha") {
    aliases <- stats::setNames(
      by_name[.islh_area_aliases],
      names(.islh_area_aliases)
    )
    values <- c(values, aliases)
  }
  values
}

# A manual scale warns only when nothing matches. One mistyped name among
# fourteen would be drawn in the missing-data grey without a word, so check
# every value the data brings.
.islh_area_limits <- function(level, keys) {
  # ggplot2 asks for the limits several times per plot; say it once.
  seen <- new.env(parent = emptyenv())
  seen$warned <- character()
  function(limits) {
    unknown <- setdiff(stats::na.omit(limits), c(keys, seen$warned))
    if (length(unknown) > 0L) {
      seen$warned <- c(seen$warned, unknown)
      label <- if (level == "lha") {
        "local health area"
      } else {
        "health service delivery area"
      }
      .islh_warn(c(
        "{cli::qty(unknown)}{.val {unknown}} {?is not an/are not} Island Health
         {label}{?/s}, so {?it is/they are} drawn in the missing-data colour.",
        i = "Use the names or codes from {.code islh_areas(\"{level}\")}."
      ))
    }
    limits
  }
}

.islh_area_scale <- function(
  aesthetic,
  level,
  na.value,
  ...,
  call = rlang::caller_env()
) {
  column <- if (aesthetic == "fill") "colour" else "line_colour"
  values <- .islh_area_values(level, column)
  keys <- names(values)
  dots <- list(...)

  fixed <- intersect(names(dots), c("values", "aesthetics"))
  if (length(fixed) > 0L) {
    .islh_abort(
      c(
        "{.arg {fixed}} cannot be changed in an Island Health area scale.",
        i = "The scale sets each area's colour itself. For other colours, use
             {.fn ggplot2::scale_fill_manual} or
             {.fn ggplot2::scale_colour_manual}."
      ),
      call = call
    )
  }

  # Breaks and limits may be set to show or keep only some areas. Names given
  # there are checked like the data, so a typo still warns rather than
  # quietly dropping an area.
  check_limits <- .islh_area_limits(level, keys)
  for (arg in intersect(names(dots), c("breaks", "limits"))) {
    if (is.character(dots[[arg]])) {
      check_limits(dots[[arg]])
    }
  }

  defaults <- list(
    values = values,
    breaks = keys,
    limits = check_limits,
    na.value = na.value,
    aesthetics = aesthetic
  )
  args <- c(dots, defaults[setdiff(names(defaults), names(dots))])
  do.call(ggplot2::scale_fill_manual, args)
}

#' Island Health colours for local health areas and HSDAs
#'
#' Colours each local health area (LHA) or health service delivery area
#' (HSDA) with its fixed brand colour from [islh_areas()]. The data can hold
#' the official names, such as `"Greater Nanaimo"`, or the codes, such as
#' `"424"`, as character or factor. The legend lists areas in code order,
#' which groups LHAs by HSDA.
#'
#' A value that matches no area is drawn in `na.value`, with a warning naming
#' it.
#'
#' The fill scale uses each area's `colour` from [islh_areas()]. The colour
#' scale, meant for lines and points, uses its `line_colour`, 10 values
#' darker, so even the lightest areas reach 3:1 against a white page.
#'
#' Colour alone cannot identify 14 areas. Label them as well, on the map or at
#' the end of each line; the brand standard requires a cue besides colour when
#' colour carries meaning.
#'
#' @param level `"lha"` for local health areas or `"hsda"` for health service
#'   delivery areas.
#' @param ... Additional arguments passed to `ggplot2::scale_fill_manual()` or
#'   `ggplot2::scale_colour_manual()`, such as `name`, `guide`, `breaks` or
#'   `limits`. `breaks` and `limits` replace the defaults, which list every
#'   area in code order; names in them are checked against the areas, like
#'   the data. `values` and `aesthetics` are fixed by the scale.
#' @param na.value Colour for missing or unmatched areas.
#'
#' @return A ggplot2 discrete scale.
#'
#' @examples
#' \dontshow{assign("font", "", envir = getFromNamespace(".islh_state", "islandbrand"))}
#' # Simulated weekly rates for every local health area.
#' areas <- islh_areas()
#' weeks <- seq(as.Date("2026-01-04"), by = "week", length.out = 10)
#' rates <- expand.grid(week = weeks, code = areas$code, stringsAsFactors = FALSE)
#' rates <- merge(rates, areas[c("code", "name", "hsda")])
#' rates$hsda <- factor(rates$hsda, levels = unique(areas$hsda))
#' level <- 8 * as.numeric(substr(rates$code, 3, 3))
#' season <- 6 * sin(as.numeric(rates$week - min(rates$week)) / 20)
#' rates$rate <- level + season + 10
#' last_week <- rates[rates$week == max(rates$week), ]
#'
#' # One panel per HSDA holds 4 to 6 lines of one colour family. Each line is
#' # named at its end, in text dark enough to read, so no reader has to match
#' # colours to a legend.
#' ggplot2::ggplot(rates, ggplot2::aes(week, rate, colour = name)) +
#'   ggplot2::geom_line(linewidth = 0.9) +
#'   ggplot2::geom_text(
#'     data = last_week,
#'     ggplot2::aes(label = name),
#'     colour = islh_hex("grey", 15),
#'     hjust = 0,
#'     nudge_x = 2,
#'     size = 3.8
#'   ) +
#'   ggplot2::facet_wrap(ggplot2::vars(hsda), ncol = 1) +
#'   ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0.02, 0.45))) +
#'   scale_colour_islh_area(guide = "none") +
#'   ggplot2::labs(x = NULL, y = "Rate per 100,000") +
#'   theme_islh()
#'
#' @export
scale_fill_islh_area <- function(
  level = c("lha", "hsda"),
  ...,
  na.value = .islh_map_missing()
) {
  level <- match.arg(level)
  .islh_area_scale("fill", level, na.value, ...)
}

#' @rdname scale_fill_islh_area
#' @export
scale_colour_islh_area <- function(
  level = c("lha", "hsda"),
  ...,
  na.value = .islh_unknown()
) {
  level <- match.arg(level)
  .islh_area_scale("colour", level, na.value, ...)
}

#' @rdname scale_fill_islh_area
#' @export
scale_color_islh_area <- scale_colour_islh_area

# Where each LHA's code goes on a map in BC Albers (EPSG:3005), in metres.
#
# Eight areas hold their code: the label sits inside, at a point chosen by eye
# from sf::st_point_on_surface() of the catalogue boundaries. Six are too small
# or narrow (411, 413, 414, 421, 423, 425). Their codes sit in a column over
# the Strait of Georgia, east of the Island, with a leader line from a point
# inside each area. The column runs north to south in the same order as the
# anchors, so no two leader lines cross. tests/testthat/test-areas.R checks
# all of this against islh_example_lha().
.islh_lha_label_positions <- data.frame(
  code = c(
    "411",
    "412",
    "413",
    "414",
    "421",
    "422",
    "423",
    "424",
    "425",
    "426",
    "431",
    "432",
    "433",
    "434"
  ),
  label_x = c(
    1228000,
    1148000,
    1228000,
    1228000,
    1228000,
    1127000,
    1228000,
    1133000,
    1228000,
    1055000,
    1060000,
    1031000,
    978000,
    1000000
  ),
  label_y = c(
    378000,
    391000,
    400000,
    446000,
    422000,
    424000,
    470000,
    458000,
    500000,
    458000,
    516000,
    552000,
    540000,
    700000
  ),
  anchor_x = c(
    1194162,
    NA,
    1190608,
    1182970,
    1167741,
    NA,
    1163000,
    NA,
    1115000,
    NA,
    NA,
    NA,
    NA,
    NA
  ),
  anchor_y = c(
    386787,
    NA,
    400719,
    426480,
    413703,
    NA,
    438000,
    NA,
    478000,
    NA,
    NA,
    NA,
    NA,
    NA
  ),
  stringsAsFactors = FALSE
)

#' Label local health areas on a map without overlaps
#'
#' Adds each local health area's code to a map of Island Health in BC Albers,
#' such as [islh_example_lha()] or a boundary download from islandepi. Eight
#' areas are large enough to hold their code, which sits inside in the area's
#' `label_colour` from [islh_areas()]. The other six (411 Greater Victoria,
#' 413 Saanich Peninsula, 414 Southern Gulf Islands, 421 Cowichan Valley South,
#' 423 Cowichan Valley North and 425 Oceanside) are labelled in a column over
#' the Strait of Georgia, each joined to its area by a thin leader line, so no
#' code overlaps another or crosses a border.
#'
#' The positions are fixed for the whole Island and suit figures 6 inches wide
#' or more. On a map zoomed to part of it, such as an inset, label the areas
#' yourself.
#'
#' @param size Text size in millimetres, as for [ggplot2::geom_text()]. The
#'   default of 4 mm is about 11 points. Codes do not rely on being large:
#'   each sits on its area's fill in its `label_colour`, or on the white page
#'   in Grey 10, and every pair reaches 4.5:1, the WCAG minimum for text of
#'   any size.
#'
#' @return A list of ggplot2 layers to add to a map with `+`.
#'
#' @seealso [scale_fill_islh_area()] for each LHA's colour.
#'
#' @examples
#' \dontshow{assign("font", "", envir = getFromNamespace(".islh_state", "islandbrand"))}
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   lha <- islh_example_lha()
#'
#'   ggplot2::ggplot(lha) +
#'     ggplot2::geom_sf(
#'       ggplot2::aes(fill = geography_name),
#'       colour = "white",
#'       linewidth = 0.3
#'     ) +
#'     islh_area_labels() +
#'     scale_fill_islh_area(guide = "none") +
#'     coord_islh_map() +
#'     theme_islh_map()
#' }
#'
#' @export
islh_area_labels <- function(size = 4) {
  size <- .islh_check_size(size, "size", maximum = 20)

  labels <- merge(
    .islh_lha_label_positions,
    islh_areas()[c("code", "label_colour")],
    by = "code",
    sort = TRUE
  )
  callout <- !is.na(labels$anchor_x)
  inside <- labels[!callout, , drop = FALSE]
  outside <- labels[callout, , drop = FALSE]

  # The leader stops short of the text so the two do not touch.
  gap <- 3000
  dark <- islh_hex("grey", 10)

  # A text layer widens the map to where each callout starts, not to where its
  # text ends, so reserve room for the code itself or it is cut off.
  room <- data.frame(
    x = max(outside$label_x) + 9500 * size,
    y = range(outside$label_y) + c(-2500, 2500) * size
  )

  list(
    ggplot2::geom_blank(
      data = room,
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      inherit.aes = FALSE
    ),
    ggplot2::geom_segment(
      data = outside,
      mapping = ggplot2::aes(
        x = .data$anchor_x,
        y = .data$anchor_y,
        xend = .data$label_x - gap,
        yend = .data$label_y
      ),
      colour = islh_hex("grey", 40),
      linewidth = 0.3,
      inherit.aes = FALSE
    ),
    ggplot2::geom_point(
      data = outside,
      mapping = ggplot2::aes(x = .data$anchor_x, y = .data$anchor_y),
      colour = dark,
      size = 0.8,
      inherit.aes = FALSE
    ),
    ggplot2::geom_text(
      data = inside,
      mapping = ggplot2::aes(
        x = .data$label_x,
        y = .data$label_y,
        label = .data$code
      ),
      colour = inside$label_colour,
      fontface = "bold",
      size = size,
      family = .islh_font(),
      inherit.aes = FALSE
    ),
    # Callouts sit on the white page, where dark text meets the brand's
    # small-text contrast at any size.
    ggplot2::geom_text(
      data = outside,
      mapping = ggplot2::aes(
        x = .data$label_x,
        y = .data$label_y,
        label = .data$code
      ),
      colour = dark,
      hjust = 0,
      fontface = "bold",
      size = size,
      family = .islh_font(),
      inherit.aes = FALSE
    )
  )
}
