# A temporary customdata field carries row identities through Plotly's
# grouping, faceting and polygon expansion. The finished traces recover any
# original customdata and use the reporting values for their hover text.
.islh_prepare_epi_hover <- function(plot, tooltip) {
  state <- list(
    ids = numeric(),
    text = character(),
    skip = logical(),
    layer = integer(),
    row = integer(),
    customdata = vector("list", length(plot$layers))
  )
  for (i in seq_along(plot$layers)) {
    layer <- plot$layers[[i]]
    spec <- attr(layer, "islh_hover", exact = TRUE)
    if (is.null(spec) || !is.data.frame(layer$data) || nrow(layer$data) == 0L) {
      next
    }
    rows <- seq_len(nrow(layer$data))
    # Numeric identities do not introduce discrete groups into ribbons or lines.
    ids <- -(1e12 + length(state$ids) + rows)
    customdata <- layer$mapping$customdata
    if (is.null(customdata)) {
      customdata <- plot$mapping$customdata
    }
    if (!is.null(customdata)) {
      original <- rlang::eval_tidy(customdata, layer$data)
      if (length(original) == 1L) {
        original <- rep(original, length(rows))
      }
      state$customdata[i] <- list(original)
    }
    text <- rep("", length(rows))
    fields <- spec$fields
    if (!any(c("all", "text") %in% tooltip)) {
      fields <- fields[names(fields) %in% tooltip | fields %in% tooltip]
    }
    for (aesthetic in names(fields)) {
      field <- fields[[aesthetic]]
      value <- layer$data[[field]]
      if (aesthetic == "x" && !is.null(spec$dates)) {
        value <- spec$dates
      }
      label <- .islh_epi_hover_label(plot, aesthetic, field, spec$kind)
      value <- .islh_epi_hover_value(
        value,
        reference = spec$kind == "reference"
      )
      line <- paste0(
        htmltools::htmlEscape(label),
        ": ",
        htmltools::htmlEscape(value)
      )
      text <- paste0(
        text,
        if (aesthetic == names(fields)[[1]]) "" else "<br>",
        line
      )
    }
    state$ids <- c(state$ids, ids)
    state$text <- c(state$text, text)
    state$skip <- c(
      state$skip,
      rep(
        spec$kind == "skip" || identical(tooltip, "none") || !length(fields),
        length(rows)
      )
    )
    state$layer <- c(state$layer, rep(i, length(rows)))
    state$row <- c(state$row, rows)
    # Clone the ggproto layer so converting a plot cannot modify its source.
    layer <- .islh_clone_hover_layer(layer)
    layer$data$.islh_hover_id <- ids
    layer$mapping$customdata <- ggplot2::aes(
      customdata = .data$.islh_hover_id
    )$customdata
    plot$layers[[i]] <- layer
  }
  list(plot = plot, state = state)
}

.islh_clone_hover_layer <- function(original) {
  force(original)
  ggplot2::ggproto(NULL, original)
}

.islh_epi_hover_label <- function(plot, aesthetic, field, kind) {
  if (kind == "reference" && aesthetic %in% c("y", "ymin", "ymax")) {
    return(switch(
      aesthetic,
      y = "Reference mean",
      ymin = "Lower limit",
      ymax = "Upper limit"
    ))
  }
  fallback <- switch(aesthetic, x = "Date", y = "Cases", field)
  label <- plot$labels[[aesthetic]]
  if (
    !is.character(label) ||
      length(label) != 1L ||
      is.na(label) ||
      !nzchar(label)
  ) {
    label <- fallback
  }
  if (kind == "total" && aesthetic == "y") {
    label <- if (label == "Cases") "Total cases" else paste0(label, " (total)")
  }
  label
}

.islh_epi_hover_value <- function(value, reference = FALSE) {
  out <- if (inherits(value, "Date")) {
    format(value, "%Y-%m-%d")
  } else if (is.numeric(value)) {
    if (reference) {
      value <- round(value, 2)
    }
    vapply(value, format, "", trim = TRUE, scientific = FALSE, big.mark = ",")
  } else {
    as.character(value)
  }
  out[is.na(value)] <- "Missing"
  out
}

.islh_finish_epi_hover <- function(widget, state) {
  widget$x$data <- lapply(widget$x$data, function(trace) {
    ids <- trace$customdata
    if (!is.numeric(ids)) {
      return(trace)
    }
    indexes <- match(ids, state$ids)
    present <- !is.na(ids)
    if (!any(present) || anyNA(indexes[present])) {
      return(trace)
    }
    layer <- state$layer[indexes[which(present)[[1]]]]
    original <- state$customdata[[layer]]
    trace$customdata <- if (is.null(original)) {
      NULL
    } else {
      original[state$row[indexes]]
    }
    skip <- all(state$skip[indexes], na.rm = TRUE)
    trace$hoverinfo <- if (skip) "skip" else "text"
    trace$hovertemplate <- if (skip) NULL else "%{hovertext}<extra></extra>"
    trace$hovertext <- if (skip) NULL else unname(state$text[indexes])
    if (identical(trace$hoveron, "fills") && !skip) {
      # Plotly's area-hover path reads one scalar text value, ignoring the
      # point hover template. Each case polygon describes one reporting row.
      trace$text <- unique(stats::na.omit(trace$hovertext))[[1]]
      trace$hovertemplate <- NULL
    } else if (
      is.null(trace$mode) || !grepl("text", trace$mode, fixed = TRUE)
    ) {
      trace$text <- NULL
    }
    trace
  })
  widget
}
