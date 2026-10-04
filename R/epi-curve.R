#' Draw an Island Health epidemic curve
#'
#' `islh_epi_curve()` turns prepared period counts into a branded surveillance
#' figure. It deliberately keeps counting separate from plotting: use
#' `islandepi::islh_count_events()` first when starting from event-level data.
#'
#' @param data A data frame containing one row per plotted group and period.
#' @param date Date column.
#' @param count Whole-count column.
#' @param fill Optional categorical column used to stack or dodge bars.
#' @param facet Optional column used to create small multiples.
#' @param aggregate Sum counts that share a date, fill group and facet. With
#'   `FALSE`, the default, duplicated combinations are an error. See the input
#'   grain section.
#' @param style `"bars"` for routine surveillance or `"cases"` to draw one
#'   outlined rectangle per case. Case tiles are intended for small outbreaks.
#' @param position Stack or dodge grouped bars. Case tiles require stacking.
#' @param labels Show no labels or total counts above each period.
#' @param reference Optional data frame containing a date column and one or more
#'   of `lower_limit`, `upper_limit` and `reference_mean`, with one row per
#'   date. When it also contains the `facet` column it needs one row per date
#'   and facet, and each panel shows its own reference. Without the facet
#'   column, the same reference is drawn in every panel. Extra rows, such as a
#'   reference for each fill group, are an error.
#' @param reference_date Date column in `reference`. Defaults to the name used
#'   by `date` when that column is present.
#' @param lower,upper,reference_mean Column names used for the reference ribbon
#'   and line. Missing optional columns are ignored.
#' @param show_year_lines Draw a separator at the beginning of each new year.
#' @param bar_width Width in days, measured from each period's start. When
#'   `NULL`, the function uses nine tenths of the shortest distance between
#'   periods. With a single period it uses nine tenths of the period's length
#'   when `data` comes from `islandepi::islh_count_events()`, which records the
#'   interval, and 0.9 days otherwise. Start dates alone cannot show a missing
#'   period or months of different lengths, so set `bar_width` for irregular
#'   periods.
#' @param date_breaks,date_labels Values passed to `ggplot2::scale_x_date()`.
#'   Useful defaults are selected when omitted.
#' @param max_cases Maximum number of rectangles allowed with
#'   `style = "cases"`.
#' @param title,subtitle,caption,x,y Plot labels.
#' @param facet_scales Scale behaviour passed to `ggplot2::facet_wrap()`.
#'
#' @return A ggplot object. Additional ggplot2 layers and scales can be added in
#'   the usual way.
#'
#' @section Date alignment:
#'
#' Each date is read as the start of its period, and each bar covers the
#' period from that date forward, the usual convention for an epidemic curve.
#' A week starting on Sunday 28 December is drawn from 28 December towards
#' 3 January, not centred on 28 December with half of it in the week before.
#' Case tiles, total labels and the reference band and line follow the bars.
#'
#' Date-times are converted to dates in their own time zone, so a report
#' timestamped in Pacific time is counted on the Pacific date.
#'
#' @section Input grain:
#'
#' The function expects at most one row for each combination of date, fill
#' group and facet. Two rows for the same combination are not drawn as two
#' rows: `geom_col()` stacks them into a single taller bar, and `style =
#' "cases"` draws overlapping tiles. Either way the figure is wrong and nothing
#' in it says so, which is why duplicates stop rather than plot.
#'
#' Duplicates usually mean the data has not been counted yet, so the fix is
#' normally to count it: `islandepi::islh_count_events()` returns one row per
#' period and group, and fills in the periods with no events, which a plot
#' cannot infer. Use `aggregate = TRUE` when the rows really are separate
#' counts of the same period that should be added together. Columns outside the
#' date, fill and facet keep the first row's value.
#'
#' Rows are grouped by the values themselves, so `"A.B"` with `"C"` and `"A"`
#' with `"B.C"` stay apart. A missing fill or facet value is a group of its
#' own: it is summed, stacked and labelled like any other value, and a missing
#' facet gets its own panel.
#'
#' @section Zero counts:
#'
#' Keep rows with a count of zero. Both styles keep their periods on the date
#' axis, their panels and their fill groups, so a quiet area is shown as quiet
#' rather than left out. A period with no row at all is not drawn as zero:
#' the function cannot tell an absent row from a period nobody reported on.
#'
#' @examples
#' \dontshow{assign("font", "", envir = getFromNamespace(".islh_state", "islandbrand"))}
#' curve_data <- data.frame(
#'   week = seq(as.Date("2026-01-04"), by = "week", length.out = 12),
#'   cases = c(1, 2, 3, 5, 8, 13, 10, 7, 5, 3, 2, 1),
#'   source = rep(c("Community", "Facility"), 6)
#' )
#'
#' islh_epi_curve(
#'   curve_data,
#'   date = week,
#'   count = cases,
#'   fill = source,
#'   title = "Reported cases by week"
#' )
#'
#' @export
islh_epi_curve <- function(
  data,
  date,
  count,
  fill = NULL,
  facet = NULL,
  style = c("bars", "cases"),
  position = c("stack", "dodge"),
  labels = c("none", "total"),
  reference = NULL,
  reference_date = NULL,
  lower = "lower_limit",
  upper = "upper_limit",
  reference_mean = "reference_mean",
  show_year_lines = TRUE,
  bar_width = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  max_cases = 50000L,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  x = NULL,
  y = "Cases",
  facet_scales = "fixed",
  aggregate = FALSE
) {
  if (!is.data.frame(data)) {
    .islh_abort("{.arg data} must be a data frame.")
  }
  if (nrow(data) == 0L) {
    .islh_abort("{.arg data} must contain at least one row.")
  }
  style <- match.arg(style)
  position <- match.arg(position)
  labels <- match.arg(labels)
  show_year_lines <- .islh_check_flag(show_year_lines, "show_year_lines")
  aggregate <- .islh_check_flag(aggregate, "aggregate")

  date_name <- .islh_plot_column(data, rlang::enquo(date), "date")
  count_name <- .islh_plot_column(data, rlang::enquo(count), "count")
  fill_name <- .islh_plot_column(
    data,
    rlang::enquo(fill),
    "fill",
    optional = TRUE
  )
  facet_name <- .islh_plot_column(
    data,
    rlang::enquo(facet),
    "facet",
    optional = TRUE
  )

  interval <- attr(data, "islh_interval", exact = TRUE)
  plot_data <- as.data.frame(data)
  plot_data[[date_name]] <- .islh_plot_dates(plot_data[[date_name]], date_name)
  plot_data[[count_name]] <- .islh_plot_counts(
    plot_data[[count_name]],
    count_name
  )
  plot_data <- .islh_plot_grain(
    plot_data,
    date_name = date_name,
    count_name = count_name,
    fill_name = fill_name,
    facet_name = facet_name,
    aggregate = aggregate
  )

  if (is.null(bar_width)) {
    unique_dates <- sort(unique(plot_data[[date_name]]))
    if (length(unique_dates) < 2L) {
      bar_width <- 0.9 * .islh_period_days(unique_dates, interval)
    } else {
      bar_width <- 0.9 * min(as.numeric(diff(unique_dates)))
    }
  }
  # A bar width is measured in days along the date axis, not in inches.
  if (
    !is.numeric(bar_width) ||
      length(bar_width) != 1L ||
      is.na(bar_width) ||
      !is.finite(bar_width) ||
      bar_width <= 0
  ) {
    .islh_abort("{.arg bar_width} must be one positive finite number of days.")
  }
  max_cases <- .islh_check_count(max_cases, "max_cases")

  # Each bar spans its period rather than being centred on its start date, so
  # everything that marks a period is drawn at the middle of its bar.
  offset <- bar_width / 2
  plot_data$.islh_x <- plot_data[[date_name]] + offset

  plot <- ggplot2::ggplot()

  reference_layers <- .islh_plot_reference(
    reference = reference,
    data = plot_data,
    data_date_name = date_name,
    facet_name = facet_name,
    reference_date = reference_date,
    lower = lower,
    upper = upper,
    reference_mean = reference_mean,
    offset = offset
  )
  if (!is.null(reference_layers$ribbon)) {
    plot <- plot + reference_layers$ribbon
  }
  if (!is.null(reference_layers$line)) {
    plot <- plot + reference_layers$line
  }

  date_range <- range(plot_data[[date_name]])
  if (isTRUE(show_year_lines)) {
    first_year <- as.integer(format(date_range[1], "%Y"))
    last_year <- as.integer(format(date_range[2], "%Y"))
    if (last_year > first_year) {
      year_starts <- as.Date(paste0(
        seq.int(first_year + 1L, last_year),
        "-01-01"
      ))
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = year_starts,
          colour = islh_hex("grey", 40),
          linewidth = 0.35,
          linetype = "dashed"
        )
    }
  }

  mapping <- if (is.null(fill_name)) {
    ggplot2::aes(x = .data$.islh_x, y = .data[[count_name]])
  } else {
    ggplot2::aes(
      x = .data$.islh_x,
      y = .data[[count_name]],
      fill = .data[[fill_name]]
    )
  }

  if (style == "bars") {
    bar_position <- if (position == "stack") {
      "stack"
    } else {
      ggplot2::position_dodge2(width = bar_width, preserve = "single")
    }
    bar_args <- list(
      data = plot_data,
      mapping = mapping,
      width = bar_width,
      position = bar_position,
      colour = "white",
      linewidth = 0.2
    )
    if (is.null(fill_name)) {
      bar_args$fill <- islh_brand("primary")
    }
    bar <- do.call(ggplot2::geom_col, bar_args)
    plot <- plot + bar
  } else {
    if (position != "stack") {
      .islh_abort("{.code style = \"cases\"} only supports stacked groups.")
    }
    # Tiles are drawn only for cases, so a period or panel with none would
    # vanish from the figure. Zero is a result: this blank layer keeps every
    # period on the date axis, every panel and every fill group in the legend,
    # as the bars do.
    skeleton <- plot_data
    skeleton$.islh_xmin <- skeleton[[date_name]]
    skeleton$.islh_xmax <- skeleton[[date_name]] + bar_width
    skeleton_mapping <- if (is.null(fill_name)) {
      ggplot2::aes(
        xmin = .data$.islh_xmin,
        xmax = .data$.islh_xmax,
        y = .data[[count_name]]
      )
    } else {
      ggplot2::aes(
        xmin = .data$.islh_xmin,
        xmax = .data$.islh_xmax,
        y = .data[[count_name]],
        fill = .data[[fill_name]]
      )
    }
    plot <- plot +
      ggplot2::geom_blank(data = skeleton, mapping = skeleton_mapping)
    case_data <- .islh_expand_cases(
      plot_data,
      date_name = date_name,
      count_name = count_name,
      fill_name = fill_name,
      facet_name = facet_name,
      max_cases = max_cases
    )
    case_mapping <- if (is.null(fill_name)) {
      ggplot2::aes(x = .data$.islh_x, y = .data$.islh_case_y)
    } else {
      ggplot2::aes(
        x = .data$.islh_x,
        y = .data$.islh_case_y,
        fill = .data[[fill_name]]
      )
    }
    case_args <- list(
      data = case_data,
      mapping = case_mapping,
      width = bar_width,
      height = 1,
      colour = "white",
      linewidth = 0.25
    )
    if (is.null(fill_name)) {
      case_args$fill <- islh_brand("primary")
    }
    cases <- do.call(ggplot2::geom_tile, case_args)
    plot <- plot + cases
  }

  if (!is.null(fill_name)) {
    plot <- plot + scale_fill_islh()
  }

  if (labels == "total") {
    totals <- .islh_plot_totals(
      plot_data,
      date_name = date_name,
      count_name = count_name,
      facet_name = facet_name
    )
    totals$.islh_date <- totals$.islh_date + offset
    label_mapping <- ggplot2::aes(
      x = .data$.islh_date,
      y = .data$.islh_total,
      label = .data$.islh_total
    )
    plot <- plot +
      ggplot2::geom_text(
        data = totals,
        mapping = label_mapping,
        inherit.aes = FALSE,
        vjust = -0.35,
        # 11 pt bold: read at a glance, and a step below the 12 pt axis text
        # of the default theme so the totals do not compete with the title.
        size = 11,
        size.unit = "pt",
        fontface = "bold",
        family = .islh_font(),
        colour = islh_brand("black")
      )
  }

  if (is.null(date_breaks)) {
    span <- as.numeric(diff(date_range))
    date_breaks <- if (span <= 45) {
      "1 week"
    } else if (span <= 180) {
      "1 month"
    } else {
      "3 months"
    }
  }
  if (is.null(date_labels)) {
    date_labels <- if (as.numeric(diff(date_range)) <= 370) {
      "%b %d"
    } else {
      "%b\n%Y"
    }
  }

  plot <- plot +
    ggplot2::scale_x_date(
      date_breaks = date_breaks,
      date_labels = date_labels,
      expand = ggplot2::expansion(mult = c(0.01, 0.02))
    ) +
    # Labelled totals sit above the tallest bar, so they need headroom that
    # the usual 5% does not give.
    scale_y_islh_count(
      expand = ggplot2::expansion(
        mult = c(0, if (labels == "total") 0.1 else 0.05)
      )
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = x,
      y = y,
      fill = if (is.null(fill_name)) NULL else fill_name
    ) +
    theme_islh() +
    # The last date label is centred on the panel edge, so half of it hangs
    # past the panel. Room on the right keeps it from being cut off.
    ggplot2::theme(plot.margin = ggplot2::margin(6, 24, 6, 6))

  if (!is.null(facet_name)) {
    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(!!rlang::sym(facet_name)),
        scales = facet_scales
      )
  }

  plot
}

.islh_plot_column <- function(
  data,
  quo,
  arg,
  optional = FALSE,
  call = rlang::caller_env()
) {
  if (rlang::quo_is_null(quo)) {
    if (isTRUE(optional)) {
      return(NULL)
    }
    .islh_abort("{.arg {arg}} must select one column.", call = call)
  }
  expression <- rlang::get_expr(quo)
  name <- if (rlang::is_symbol(expression)) {
    rlang::as_name(expression)
  } else if (is.character(expression) && length(expression) == 1L) {
    expression
  } else {
    .islh_abort(
      "{.arg {arg}} must be a bare column name or one string.",
      call = call
    )
  }
  if (!name %in% names(data)) {
    .islh_abort(
      "Column {.field {name}} selected by {.arg {arg}} was not found.",
      call = call
    )
  }
  name
}

# One row per date, fill group and facet.
#
# geom_col() stacks two rows for the same combination into one taller bar, and
# the case style draws its tiles on top of each other. Both produce a figure
# that looks finished and is wrong, so duplicates stop here unless the caller
# asks for them to be summed.
.islh_plot_grain <- function(
  data,
  date_name,
  count_name,
  fill_name,
  facet_name,
  aggregate,
  call = rlang::caller_env()
) {
  keys <- c(date_name, fill_name, facet_name)
  ids <- .islh_group_ids(data, keys)
  repeated <- duplicated(ids)
  if (!any(repeated)) {
    return(data)
  }

  if (!isTRUE(aggregate)) {
    n <- sum(repeated)
    .islh_abort(
      c(
        "{.arg data} has more than one row for the same period.",
        x = "Found {n} repeated combination{?s} of {.field {keys}}.",
        i = "Count the events first with
           {.code islandepi::islh_count_events()}, which also fills in periods
           with no events, or pass {.code aggregate = TRUE} to add the
           duplicated rows together."
      ),
      call = call
    )
  }

  out <- data[!repeated, , drop = FALSE]
  out[[count_name]] <- .islh_group_sums(data[[count_name]], ids)
  rownames(out) <- NULL
  out
}

.islh_plot_dates <- function(x, arg, call = rlang::caller_env()) {
  if (inherits(x, "Date")) {
    out <- as.Date(x)
  } else if (inherits(x, "POSIXct")) {
    # `as.Date()` reads a POSIXct in UTC unless told otherwise, before R 4.3,
    # which moves an evening in Pacific time onto the next day. Use the zone
    # the times were recorded in; an empty zone means the session's own.
    zone <- attr(x, "tzone")
    zone <- if (is.null(zone) || !nzchar(zone[[1]])) "" else zone[[1]]
    out <- as.Date(as.POSIXlt(x, tz = zone))
  } else if (inherits(x, "POSIXlt")) {
    out <- as.Date(x)
  } else if (is.character(x) || is.factor(x)) {
    text <- trimws(as.character(x))
    shape_ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text)
    out <- suppressWarnings(as.Date(text, format = "%Y-%m-%d"))
    valid <- !is.na(out) & shape_ok & format(out, "%Y-%m-%d") == text
    out[!valid] <- as.Date(NA)
  } else {
    .islh_abort(
      "{.arg {arg}} must contain dates, not {.cls {class(x)[1]}}.",
      call = call
    )
  }
  if (anyNA(out)) {
    .islh_abort(
      c(
        "{.arg {arg}} must not contain missing or invalid dates.",
        i = "Use Date values or ISO dates written as YYYY-MM-DD."
      ),
      call = call
    )
  }
  out
}

.islh_plot_counts <- function(x, arg, call = rlang::caller_env()) {
  if (
    is.factor(x) ||
      !is.numeric(x) ||
      anyNA(x) ||
      any(!is.finite(x)) ||
      any(x < 0) ||
      any(abs(x - round(x)) > .Machine$double.eps^0.5)
  ) {
    .islh_abort(
      "{.arg {arg}} must contain non-negative finite whole counts.",
      call = call
    )
  }
  as.numeric(x)
}

.islh_plot_reference <- function(
  reference,
  data,
  data_date_name,
  facet_name,
  reference_date,
  lower,
  upper,
  reference_mean,
  offset = 0,
  call = rlang::caller_env()
) {
  empty <- list(ribbon = NULL, line = NULL)
  if (is.null(reference)) {
    return(empty)
  }
  if (!is.data.frame(reference) || nrow(reference) == 0L) {
    .islh_abort(
      "{.arg reference} must be NULL or a non-empty data frame.",
      call = call
    )
  }
  reference <- as.data.frame(reference)
  if (is.null(reference_date)) {
    reference_date <- if (data_date_name %in% names(reference)) {
      data_date_name
    } else if ("period_start" %in% names(reference)) {
      "period_start"
    } else {
      .islh_abort(
        "Supply {.arg reference_date}; no matching date column was found.",
        call = call
      )
    }
  }
  if (
    !is.character(reference_date) ||
      length(reference_date) != 1L ||
      !reference_date %in% names(reference)
  ) {
    .islh_abort(
      "{.arg reference_date} must name a column in {.arg reference}.",
      call = call
    )
  }
  reference[[reference_date]] <- .islh_plot_dates(
    reference[[reference_date]],
    "reference_date",
    call = call
  )
  .islh_check_reference_grain(
    reference,
    data,
    reference_date = reference_date,
    facet_name = facet_name,
    call = call
  )
  # Reference values describe a whole period, so they sit mid-bar.
  reference[[reference_date]] <- reference[[reference_date]] + offset

  fields <- list(
    lower = lower,
    upper = upper,
    reference_mean = reference_mean
  )
  for (field in names(fields)) {
    value <- fields[[field]]
    if (
      !is.null(value) &&
        (!is.character(value) || length(value) != 1L || is.na(value))
    ) {
      .islh_abort(
        "{.arg {field}} must be NULL or one column name.",
        call = call
      )
    }
  }

  has_lower <- !is.null(lower) && lower %in% names(reference)
  has_upper <- !is.null(upper) && upper %in% names(reference)
  has_mean <- !is.null(reference_mean) && reference_mean %in% names(reference)
  if (xor(has_lower, has_upper)) {
    .islh_abort(
      "{.arg reference} must contain both lower and upper limits for a ribbon.",
      call = call
    )
  }
  if (!has_lower && !has_upper && !has_mean) {
    .islh_abort(
      "{.arg reference} contains no reference limits or mean column.",
      call = call
    )
  }

  ribbon <- NULL
  if (has_lower && has_upper) {
    limits <- c(reference[[lower]], reference[[upper]])
    if (
      !is.numeric(limits) ||
        anyNA(limits) ||
        any(!is.finite(limits)) ||
        any(reference[[lower]] > reference[[upper]])
    ) {
      .islh_abort(
        "Reference limits must be finite and lower must not exceed upper.",
        call = call
      )
    }
    ribbon <- ggplot2::geom_ribbon(
      data = reference,
      mapping = ggplot2::aes(
        x = .data[[reference_date]],
        ymin = .data[[lower]],
        ymax = .data[[upper]]
      ),
      inherit.aes = FALSE,
      fill = islh_hex("grey", 90),
      alpha = 0.7
    )
  }

  line <- NULL
  if (has_mean) {
    if (
      !is.numeric(reference[[reference_mean]]) ||
        anyNA(reference[[reference_mean]]) ||
        any(!is.finite(reference[[reference_mean]]))
    ) {
      .islh_abort(
        "The reference mean must be finite and non-missing.",
        call = call
      )
    }
    line <- ggplot2::geom_line(
      data = reference,
      mapping = ggplot2::aes(
        x = .data[[reference_date]],
        y = .data[[reference_mean]],
        group = 1
      ),
      inherit.aes = FALSE,
      colour = islh_hex("grey", 40),
      linewidth = 0.65,
      linetype = "dashed"
    )
  }

  list(ribbon = ribbon, line = line)
}

# One reference row per period, or per period and panel.
#
# The band and the line each join their rows in date order. A second row for
# the same date, such as a reference for each fill group, would be joined into
# the same line, zigzagging between the series with nothing to say so. A
# reference without the facet column is drawn in every panel.
.islh_check_reference_grain <- function(
  reference,
  data,
  reference_date,
  facet_name,
  call = rlang::caller_env()
) {
  by_facet <- !is.null(facet_name) && facet_name %in% names(reference)
  keys <- c(reference_date, if (by_facet) facet_name)
  ids <- .islh_group_ids(reference, keys)
  if (anyDuplicated(ids)) {
    n <- sum(duplicated(ids))
    grain <- if (by_facet) "date and facet" else "date"
    .islh_abort(
      c(
        "{.arg reference} has more than one row for the same {grain}.",
        x = "Found {n} repeated combination{?s} of {.field {keys}}.",
        i = "The reference draws one band and one line per panel. Keep one
             row per {grain}, for example by filtering it to the series it
             describes."
      ),
      call = call
    )
  }
  if (by_facet) {
    panels <- reference[[facet_name]]
    extra <- unique(panels[is.na(match(panels, data[[facet_name]]))])
    if (length(extra) > 0L) {
      extra <- as.character(extra)
      .islh_abort(
        c(
          "{.arg reference} has {.field {facet_name}} values that
           {.arg data} does not.",
          x = "Not in {.arg data}: {.val {extra}}.",
          i = "Each would add a panel with no counts. Check the spelling, or
               add rows with a count of zero to {.arg data}."
        ),
        call = call
      )
    }
  }
  invisible(reference)
}

# The length in days of the period starting on `start`, from the interval that
# islandepi records on its counts. Without that record a single period's length
# is unknown, and one day is used.
.islh_period_days <- function(start, interval) {
  units <- c(
    day = "day",
    week = "week",
    isoweek = "week",
    epiweek = "week",
    month = "month",
    quarter = "quarter",
    year = "year"
  )
  if (
    length(start) != 1L ||
      !is.character(interval) ||
      length(interval) != 1L ||
      !interval %in% names(units)
  ) {
    return(1)
  }
  as.numeric(diff(seq(start, by = units[[interval]], length.out = 2L)))
}

.islh_expand_cases <- function(
  data,
  date_name,
  count_name,
  fill_name,
  facet_name,
  max_cases,
  call = rlang::caller_env()
) {
  total <- sum(data[[count_name]])
  if (total > max_cases) {
    .islh_abort(
      c(
        "{.code style = \"cases\"} would draw {total} rectangles.",
        i = "Use {.code style = \"bars\"} or increase {.arg max_cases} explicitly."
      ),
      call = call
    )
  }

  # Stack the tiles the way `geom_col()` stacks bars, first fill level on top,
  # whatever order the rows arrived in. Otherwise the colours swap places from
  # one period to the next.
  if (!is.null(fill_name)) {
    level <- as.integer(addNA(as.factor(data[[fill_name]]), ifany = TRUE))
    data <- data[order(-level), , drop = FALSE]
  }

  offset <- stats::ave(
    data[[count_name]],
    .islh_group_ids(data, c(date_name, facet_name)),
    FUN = function(x) cumsum(x) - x
  )
  row_index <- rep(seq_len(nrow(data)), times = data[[count_name]])
  expanded <- data[row_index, , drop = FALSE]
  expanded$.islh_case_y <- unlist(
    Map(
      function(n, start) {
        if (n == 0) numeric() else start + seq_len(n) - 0.5
      },
      data[[count_name]],
      offset
    ),
    use.names = FALSE
  )
  rownames(expanded) <- NULL
  expanded
}

# One total per period and panel. A missing facet value is a panel of its own,
# so its total is kept rather than dropped as stats::aggregate() would.
.islh_plot_totals <- function(data, date_name, count_name, facet_name) {
  ids <- .islh_group_ids(data, c(date_name, facet_name))
  first <- !duplicated(ids)
  out <- data.frame(.islh_date = data[[date_name]][first])
  if (!is.null(facet_name)) {
    out[[facet_name]] <- data[[facet_name]][first]
  }
  out$.islh_total <- .islh_group_sums(data[[count_name]], ids)
  out
}
