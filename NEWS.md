# islandbrand 0.1.0

First public release. It includes:

* `islh_setup()`, which applies the Island Health theme to plots and tables
  for HTML or Word output. Setup either completes or changes nothing: if a
  step fails part way, the session is put back as the call found it, and the
  record from an earlier successful setup is kept for `islh_reset()`.
* Report scaffolding with a Quarto extension for HTML and Word, and tools to
  check and update an existing project without losing local edits.
  * `islh_create_report()` treats a folder holding only hidden files, such as
    `.git` or `.gitignore`, as not empty, checks every file it will write
    before writing any, and lists the files `overwrite = TRUE` replaced.
  * The project's `.gitignore` keeps the Word template in Git, so a fresh
    clone renders to Word. Projects created earlier need the line
    `!/_extensions/**/*.docx` added by hand.
  * `islh_update_project()` gives each update its own backup folder and never
    writes into an earlier one, backs up every file before replacing any, and
    leaves the manifest untouched when nothing changed.
  * Project checks hash a text file with lone carriage returns as it stands;
    such files used to hash as empty.
* Branded ggplot2 themes, colour and fill scales, epidemic curves and plot
  saving.
  * `islh_epi_curve()` groups rows by their values, so `aggregate = TRUE`
    keeps `"A.B"` with `"C"` apart from `"A"` with `"B.C"`, and a missing
    fill or facet value is a group of its own, total label included. Case
    tiles keep zero-count periods, panels and fill groups. A reference needs
    one row per date, or per date and facet, and a single period takes its
    width from the interval islandepi records.
* Map styling for Island Health geographies, with an example local health
  area map and its build record.
  * `islh_areas()` chooses each label colour by measured contrast, and every
    label reaches 4.5:1 on its fill. South Vancouver Island is Blue 45 for
    this reason. A new `line_colour` column, used by
    `scale_colour_islh_area()`, reaches 3:1 on white for every area.
  * `scale_fill_islh_area()` and `scale_colour_islh_area()` accept `breaks`
    and `limits`, and check the names in them.
* Brand colours, BC Sans, logos, a `_brand.yml` and a Word reference document.
  Links in `_brand.yml` use Blue 30, which reaches 4.5:1 on white; the
  primary Blue 50 does not.
* `flextable`, `gt` and `gtsummary` tables in the Island Health style. Every
  table function checks the declared minimum version of the package it uses,
  so an old flextable stops a direct `islh_flextable()` call with the version
  needed and the install command. `islh_gt()` embeds BC Sans unless the
  document being knitted already carries it.
* `islh_install_deps()` checks again after installing and warns when packages
  are still not ready, even with `quiet = TRUE`. The README shows how to
  install the packages `islandbrand` is built on before the package file.
* Releases publish only after the Windows and Linux builds pass their checks
  and the Windows binary installs into an empty library.
