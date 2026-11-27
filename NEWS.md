# islandbrand (development version)

## Figures

* `islh_epi_curve()` refuses `labels = "total"` with dodged `fill` groups,
  where a period total floated above bars that each show one group. It
  names a missing `date` or `count`, and stops when a reference column the
  caller named, such as `reference_mean = "mean_typo"`, is not in
  `reference`. Default reference column names may still be absent.
* `islh_area_labels()` describes `size` in millimetres, the unit it uses.
* `islh_caption()` ends every part with one full stop, so a source ending in
  a full stop no longer gives `".."`. It refuses an empty or non-text
  `source`, vector values for the optional parts, and a `width` below 1.
* Palette sizes and counts are checked as whole numbers within R's integer
  range, so `Inf`, text and very large values stop with a clear message.

## Tables and fonts

* `islh_flextable(groupname_col = )` gathers unsorted rows under one heading
  per value, in order of first appearance, as `islh_gt()` does, and shades
  the heading of a missing-value group.
* `islh_gt()` adds BC Sans to any CSS the table already carries, once,
  rather than replacing it. `islh_gt()` and `islh_flextable()` refuse input
  that is neither a table nor a data frame.
* `islh_gtsummary_gt(embed_fonts = FALSE)` no longer embeds the font when
  `islh_setup()` is active.
* Embedded fonts declare the format of the installed file, so WOFF2, WOFF
  and OpenType copies of BC Sans load in the browser.
* Widgets and documents share one font dependency name, so a page holding
  both carries the font once.

## Projects and setup

* `islh_use_quarto()` and `islh_use_brand()` return paths relative to the
  project on Windows too, check `dir`, `logos` and `overwrite`, and show
  folder names with braces as written.
* `islh_create_report()` checks `path`, `title` and `author`, only tells you
  to open an `.Rproj` file when it writes one, and the example report reads
  the CSV it copies into `data/`.
* `islh_check_project()` stops with a message when a folder sits where a
  project file belongs, and a failed manifest write is reported.
* `islh_install_deps("both")` reports missing packages for both formats.
* The install command in messages asks for binaries only on Windows and
  macOS, where CRAN publishes them, as `islh_install_deps()` already did.
* `islh_setup()` and `with_islh()` no longer load flextable and gtsummary
  just to record their settings, and `islh_reset()` restores flextable
  defaults only with settings it accepts.
* Help-page examples restore the font setting they change, so running one
  no longer turns BC Sans off for the rest of the session.
* The help for `with_islh()` says that patchwork layouts are returned as
  they are.

# islandbrand 0.1.0

First public release. It includes:

* Staff can install directly from GitHub with one `remotes::install_github()`
  command, alone or together with `islandepi`. Dependencies arrive as Windows
  binaries, outdated ones are updated, and no Rtools is needed. A release tag
  installs an approved version, and release files remain for computers
  without GitHub access.
* `islh_check()` and `islh_install_deps()` accept `interactive = TRUE` to
  check or install the optional interactive HTML packages.
* `islh_plotly()` turns existing ggplots, including epidemic curves, into
  branded interactive HTML plots with hover, zoom and legend controls.
  Epidemic-curve tooltips use the reporting period's start date and group
  count, respect plot labels, identify reference values, and omit internal
  plotting fields and year-separator tooltips. Case-tile areas show the same
  labels, reference numbers are rounded for display, and existing plot themes
  are preserved. Partial theme tweaks retain the active `islh_setup()` session
  theme, including custom text size and gridlines, or the default Island Health
  styling after `islh_reset()`. A shared fill/facet field appears only once in
  hover labels.
* `islh_reactable()` creates branded HTML tables with search, sorting and
  pagination, with optional column filters and reactable customisation.
  Missing values have an explicit label; the guide shows how to display
  suppressed counts while keeping numeric sorting.
* Interactive dependency checks and installation both select HTML at the
  console, while respecting format detection inside a report render.
* The interactive reporting guide is on the website only. It carries the
  Plotly library, which made up more than half of the installed package.
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
  and the Windows binary installs into an empty library, on R 4.4.1 and on
  the current R. The release workflow can also be run by hand as a rehearsal
  that publishes nothing.
