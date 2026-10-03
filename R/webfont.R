.islh_bc_sans_webfont_css <- function(refresh = FALSE) {
  if (
    !isTRUE(refresh) &&
      exists("webfont_css", envir = .islh_state, inherits = FALSE)
  ) {
    return(.islh_state$webfont_css)
  }

  .islh_require("base64enc", "embedding BC Sans in HTML tables")

  if (isTRUE(refresh)) {
    systemfonts::reset_font_cache()
  }
  installed <- systemfonts::system_fonts()
  faces <- data.frame(
    name = c(
      "BCSans-Regular",
      "BCSans-Italic",
      "BCSans-Bold",
      "BCSans-BoldItalic"
    ),
    style = c("normal", "italic", "normal", "italic"),
    weight = c(400L, 400L, 700L, 700L)
  )
  faces$path <- installed$path[match(faces$name, installed$name)]

  if (anyNA(faces$path)) {
    .islh_warn(c(
      "BC Sans could not be embedded in the HTML table.",
      i = "Missing font {?face/faces}: {.val {faces$name[is.na(faces$path)]}}."
    ))
    .islh_state$webfont_css <- ""
    return("")
  }

  css <- vapply(
    seq_len(nrow(faces)),
    function(i) {
      encoded <- base64enc::base64encode(faces$path[[i]])
      paste0(
        "@font-face {\n",
        "  font-family: \"BC Sans\";\n",
        "  font-style: ",
        faces$style[[i]],
        ";\n",
        "  font-weight: ",
        faces$weight[[i]],
        ";\n",
        "  src: url(\"data:font/ttf;base64,",
        encoded,
        "\") format(\"truetype\");\n",
        "}"
      )
    },
    character(1)
  )

  license_notice <- paste0(
    "/* BC Sans: Copyright 2015 Google Inc. and 2023 Province of B.C.\n",
    "   Licensed under the SIL Open Font License 1.1:\n",
    "   https://github.com/bcgov/bc-sans/blob/main/LICENSE_OFL.txt */"
  )
  .islh_state$webfont_css <- paste(c(license_notice, css), collapse = "\n")
  .islh_state$webfont_css
}

# Adds BC Sans to the document being knitted. knitr's metadata belongs to one
# render, so this runs every time: a session that renders two reports must
# register the font in both. Rendering removes duplicates by name, so a
# second call within one document costs nothing.
.islh_register_webfont_dependency <- function() {
  .islh_require("htmltools", "registering BC Sans with a Quarto HTML document")
  .islh_require("knitr", "registering BC Sans with a Quarto HTML document")

  css <- .islh_bc_sans_webfont_css()
  if (!nzchar(css)) {
    return(FALSE)
  }

  dependency_dir <- file.path(
    tempdir(),
    paste0("islandbrand-", islh_version())
  )
  dir.create(dependency_dir, recursive = TRUE, showWarnings = FALSE)
  css_file <- file.path(dependency_dir, "bc-sans.css")
  writeLines(css, css_file, useBytes = TRUE)

  dependency <- htmltools::htmlDependency(
    name = "islh-bc-sans",
    version = as.character(islh_version()),
    src = c(file = dependency_dir),
    stylesheet = basename(css_file),
    all_files = FALSE
  )
  knitr::knit_meta_add(list(dependency))
  TRUE
}
