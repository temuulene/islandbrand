# Adjacency of Island Health's local health areas, computed from the BC Data
# Catalogue boundaries (WHSE_ADMIN_BOUNDARIES.BCHA_LOCAL_HEALTH_AREA_SP) by
# intersecting each area, buffered by 200 m, with the others.
lha_neighbours <- list(
  "411" = c("412", "413"),
  "412" = c("411", "413", "421", "422"),
  "413" = c("411", "412"),
  "414" = character(),
  "421" = c("412", "422", "423"),
  "422" = c("412", "421", "423", "424", "426"),
  "423" = c("421", "422", "424"),
  "424" = c("422", "423", "425", "426"),
  "425" = c("424", "426", "431"),
  "426" = c("422", "424", "425", "431", "432", "433"),
  "431" = c("425", "426", "432"),
  "432" = c("426", "431", "433", "434"),
  "433" = c("426", "432", "434"),
  "434" = c("432", "433")
)

test_that("the 14 local health areas carry their catalogue codes and names", {
  lha <- islh_areas()
  expect_equal(nrow(lha), 14L)
  expect_equal(
    lha$code,
    c(as.character(411:414), as.character(421:426), as.character(431:434))
  )
  expect_equal(
    lha$name[lha$code %in% c("421", "422", "426")],
    c("Cowichan Valley South", "Cowichan Valley West", "Alberni/Clayoquot")
  )
  expect_equal(
    as.vector(table(lha$hsda)[unique(lha$hsda)]),
    c(4L, 6L, 4L)
  )
  expect_false(anyDuplicated(lha$colour) > 0)
})

value_of <- function(hex) {
  # Find each colour's family and value in the brand ramp.
  ramp <- unlist(islandbrand:::.islh_colours)
  hit <- names(ramp)[match(toupper(hex), toupper(ramp))]
  data.frame(
    family = sub("\\..*$", "", hit),
    value = as.numeric(sub("^.*\\.", "", hit))
  )
}

test_that("each HSDA uses one low-signal family and every fill is dark enough", {
  lha <- islh_areas()
  found <- value_of(lha$colour)
  expect_false(anyNA(found$value))

  # The brand standard keeps Red, Green and Orange for signal meanings and
  # Grey for "inactive"; Blue, Cedar and Thistle organise content.
  families <- tapply(found$family, lha$hsda, unique)
  expect_equal(
    as.vector(unlist(families[unique(lha$hsda)], use.names = FALSE)),
    c("blue", "cedar", "thistle")
  )

  # Graphics need 30 values of contrast with a white page.
  expect_true(all(found$value <= 70))

  # Large labels need 50: every fill is white-text dark or Grey 10-text light.
  expect_true(all(found$value <= 50 | found$value >= 60))
  expect_equal(
    lha$label_colour,
    ifelse(found$value <= 50, "#FFFFFF", islh_hex("grey", 10))
  )
})

test_that("neighbouring areas are far apart in value", {
  lha <- islh_areas()
  value <- stats::setNames(value_of(lha$colour)$value, lha$code)
  hsda <- stats::setNames(substr(lha$code, 1, 2), lha$code)

  for (code in names(lha_neighbours)) {
    for (neighbour in lha_neighbours[[code]]) {
      gap <- abs(value[[code]] - value[[neighbour]])
      same <- hsda[[code]] == hsda[[neighbour]]
      expect_gte(gap, if (same) 20 else 10)
      # Blue and Thistle are the pair protanopia confuses; they must never
      # meet, so South and North Vancouver Island must never touch.
      expect_false(setequal(c(hsda[[code]], hsda[[neighbour]]), c("41", "43")))
    }
  }

  # Within an HSDA, no two areas share a value.
  expect_true(all(tapply(value, hsda, function(v) !anyDuplicated(v))))
})

test_that("HSDA colours are the representative colour of each family", {
  hsda <- islh_areas("hsda")
  expect_equal(hsda$code, c("41", "42", "43"))
  expect_equal(
    hsda$colour,
    c(islh_hex("blue", 50), islh_hex("cedar", 60), islh_hex("thistle", 50))
  )
})

test_that("the scale matches names, codes and common spellings", {
  data <- data.frame(
    area = c("Greater Nanaimo", "424", "Alberni-Clayoquot", "Comox Valley"),
    y = 1:4
  )
  plot <- ggplot2::ggplot(data, ggplot2::aes(area, y, fill = area)) +
    ggplot2::geom_col() +
    scale_fill_islh_area()
  fills <- ggplot2::ggplot_build(plot)$data[[1]]$fill

  colours <- stats::setNames(islh_areas()$colour, islh_areas()$name)
  expected <- colours[c(
    "Greater Nanaimo",
    "Greater Nanaimo",
    "Alberni/Clayoquot",
    "Comox Valley"
  )]
  # geom_col() orders bars by the x factor levels, which are alphabetical.
  expect_setequal(fills, unname(expected))
})

test_that("the legend follows code order, not alphabetical order", {
  data <- data.frame(
    area = c("Vancouver Island North", "Greater Victoria", "Oceanside"),
    y = 1
  )
  plot <- ggplot2::ggplot(data, ggplot2::aes(area, y, fill = area)) +
    ggplot2::geom_col() +
    scale_fill_islh_area()
  legend <- ggplot2::get_guide_data(plot, "fill")
  expect_equal(
    legend$.label,
    c("Greater Victoria", "Oceanside", "Vancouver Island North")
  )
})

test_that("an unknown area warns once and is drawn as missing", {
  data <- data.frame(area = c("Oceansid", "Oceanside"), y = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(area, y, colour = area)) +
    ggplot2::geom_point() +
    scale_colour_islh_area()

  warnings <- character()
  built <- withCallingHandlers(
    ggplot2::ggplot_build(plot),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(grep("Oceansid", warnings), 1L)
  expect_true(.islh_unknown() %in% built$data[[1]]$colour)
})

test_that("the HSDA level works the same way", {
  data <- data.frame(area = c("42", "North Vancouver Island"), y = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(area, y, fill = area)) +
    ggplot2::geom_col() +
    scale_fill_islh_area("hsda")
  expect_setequal(
    ggplot2::ggplot_build(plot)$data[[1]]$fill,
    c(islh_hex("cedar", 60), islh_hex("thistle", 50))
  )
  expect_error(scale_fill_islh_area("chsa"))
})

test_that("every LHA has one label position", {
  labels <- .islh_lha_label_positions
  expect_setequal(labels$code, islh_areas()$code)
  expect_false(anyDuplicated(labels$code) > 0)
  # Exactly the six small or narrow areas are called out.
  expect_setequal(
    labels$code[!is.na(labels$anchor_x)],
    c("411", "413", "414", "421", "423", "425")
  )
})

label_points <- function(x, y) {
  sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = 3005)
}

test_that("labels sit inside their area, or clear of the Island", {
  skip_if_not_installed("sf")
  lha <- islh_example_lha()
  labels <- .islh_lha_label_positions
  own <- match(labels$code, lha$geography_code)
  callout <- !is.na(labels$anchor_x)

  # Codes that fit are drawn inside their own area.
  inside <- label_points(labels$label_x, labels$label_y)[!callout, ]
  expect_true(all(mapply(
    function(i, j) sf::st_within(inside[i, ], lha[j, ], sparse = FALSE)[1, 1],
    seq_len(nrow(inside)),
    own[!callout]
  )))

  # Callout codes sit over water, touching no area.
  outside <- label_points(labels$label_x, labels$label_y)[callout, ]
  expect_false(any(sf::st_intersects(outside, lha, sparse = FALSE)))

  # Each leader starts inside the area it names.
  anchors <- label_points(labels$anchor_x[callout], labels$anchor_y[callout])
  expect_true(all(mapply(
    function(i, j) sf::st_within(anchors[i, ], lha[j, ], sparse = FALSE)[1, 1],
    seq_len(nrow(anchors)),
    own[callout]
  )))
})

test_that("no two leader lines cross", {
  skip_if_not_installed("sf")
  labels <- .islh_lha_label_positions
  labels <- labels[!is.na(labels$anchor_x), ]
  leaders <- sf::st_sfc(
    lapply(seq_len(nrow(labels)), function(i) {
      sf::st_linestring(rbind(
        c(labels$anchor_x[i], labels$anchor_y[i]),
        c(labels$label_x[i], labels$label_y[i])
      ))
    }),
    crs = 3005
  )
  crossings <- sf::st_intersects(leaders, sparse = FALSE)
  diag(crossings) <- FALSE
  expect_false(any(crossings))
})

test_that("islh_area_labels adds layers that draw on an LHA map", {
  skip_if_not_installed("sf")
  layers <- islh_area_labels()
  expect_type(layers, "list")

  plot <- ggplot2::ggplot(islh_example_lha()) +
    ggplot2::geom_sf() +
    layers +
    coord_islh_map() +
    theme_islh_map()
  built <- ggplot2::ggplot_build(plot)
  text <- do.call(
    rbind,
    lapply(built$data, function(d) {
      if ("label" %in% names(d)) d[c("label", "colour")] else NULL
    })
  )
  expect_setequal(text$label, islh_areas()$code)

  expect_error(islh_area_labels(size = 0), class = "islh_error")
})
