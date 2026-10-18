# islandbrand <a href="https://temuulene.github.io/islandbrand/"><img src="man/figures/logo.png" align="right" height="139" alt="islandbrand website" /></a>

Island Health branding for R figures, tables and Quarto reports.

`islandbrand` applies the Island Health brand colour system, BC Sans typography
and report layout to ggplot2 plots, epidemic curves, `flextable` and `gt`
tables, and Quarto HTML and Word output. It also scaffolds a ready-to-render
report project.

Epidemiological calculations and surveillance data preparation are provided by
[`islandepi`](https://github.com/temuulene/islandepi). The packages can be
loaded together in the same report.

Documentation: <https://temuulene.github.io/islandbrand/>

New users should start with
[Getting started with islandbrand](https://temuulene.github.io/islandbrand/articles/islandbrand.html).
For routine surveillance plots, see the evaluated
[surveillance figures guide](https://temuulene.github.io/islandbrand/articles/surveillance.html),
which also shows how to draw confidence intervals.

## Installing

### Approved staff release

Use the Windows release approved by your team lead, paired with the approved
`islandepi` version. This follows the same release gate as
[islandepi's staff guide](https://github.com/temuulene/islandepi/blob/main/inst/INSTALL.md):
release assets appear only after package checks and a clean Windows
installation pass. A version in DESCRIPTION is not a published release.

Confirm the version is published on the
[Releases page](https://github.com/temuulene/islandbrand/releases). If there
is no published version yet, use the development route below for evaluation;
an approved staff release is not available yet.

In a fresh Windows R session, replace `x.y.z` with that approved, published
version and run:

```r
version <- "x.y.z"
binary <- tempfile(fileext = ".zip")
download.file(
  sprintf(
    "https://github.com/temuulene/islandbrand/releases/download/v%s/islandbrand_%s.zip",
    version, version
  ),
  binary, mode = "wb"
)
install.packages(
  c("cli", "ggplot2", "rlang", "scales", "systemfonts"),
  type = "binary"
)
install.packages(binary, repos = NULL, type = "win.binary")
stopifnot(as.character(packageVersion("islandbrand")) == version)
```

The commands download the release automatically and install or update its
required CRAN packages as Windows binaries. No Rtools, Git client, GitHub
account, token, or manual ZIP download is needed. Downloading the published
asset directly also avoids GitHub's shared anonymous API request limit.

Then install the packages your reports need, and restart R:

```r
islandbrand::islh_install_deps("html")
```

Use `"docx"` for Word, or `"both"` for HTML and Word. Add
`interactive = TRUE` only for interactive HTML reports. Update by restarting
R and rerunning the commands with the newly approved release version.
Record both package versions and `sessionInfo()` with the report; use a
project `renv` lockfile when exact dependency versions must be reproduced.

### Development and evaluation from GitHub

To try changes before a release, install the development branch directly.
This bypasses the release gate and is for evaluation. On Windows:

```r
install.packages("remotes", type = "binary")
options(pkgType = "binary")
remotes::install_github(
  "temuulene/islandbrand",
  ref = "main",
  dependencies = NA,
  upgrade = "always",
  build = FALSE,
  build_vignettes = FALSE
)
```

`upgrade = "always"` updates required dependencies, including an installed
ggplot2 older than the minimum version. On Windows they remain prebuilt CRAN
binaries. To reproduce a reviewed development revision, replace `"main"`
with its full commit identifier; for a released revision use its actual
published tag. Unauthenticated GitHub API installs can share a rate limit
on a workplace network, so use the direct release download for staff rollout.

The package itself has no compiled code. The development install also skips
building documentation articles locally, so neither Rtools nor Pandoc or
Quarto is needed for installation. The
[online guides](https://temuulene.github.io/islandbrand/) are available to
staff. Install Quarto separately to render Quarto reports.

### Offline installation

Ask your team lead which version is approved, and get the Windows `.zip` for
it. Each published release attaches one on the
[Releases page](https://github.com/temuulene/islandbrand/releases), with a
`.tar.gz` source package. Until a version is published there, the team lead
provides the file. No token, GitHub account or compiler is needed.

R must load the packages `islandbrand` is built on before any of its
functions can run, so install those first, then the file:

```r
install.packages(
  c("cli", "ggplot2", "rlang", "scales", "systemfonts"),
  type = "binary"
)
install.packages("path/to/islandbrand_x.y.z.zip", repos = NULL)
```

Replace `x.y.z` with the approved version. PHASE staff can instead use
`install-phase.R` from the paired
[islandepi release](https://github.com/temuulene/islandepi/releases). It does
both steps, checks the version, and confirms the package loads.

Then install the packages your output format needs, and restart R:

```r
islandbrand::islh_install_deps("html")   # "docx", or "both"
```

## Choose the function

| Task | Function |
|---|---|
| Routine surveillance or epidemic curve | `islh_epi_curve()` |
| Branded ggplot | `theme_islh()` and `scale_*_islh()` |
| Interactive HTML plot | `islh_plotly()` |
| Searchable, sortable HTML table | `islh_reactable()` |
| HTML table | `islh_gt()` |
| Word table | `islh_flextable()` |
| Quarto report project | `islh_create_report()` |
| Health-geography map | `theme_islh_map()` and `coord_islh_map()` |

## Interactive HTML reports

Install the optional packages once, then restart R:

```r
islandbrand::islh_install_deps("html", interactive = TRUE)
```

In a Quarto or R Markdown HTML report, make a plot as usual and pass it to
`islh_plotly()`. Pass a data frame to `islh_reactable()` for search, sorting
and pagination:

```r
library(islandbrand)

weekly <- data.frame(
  week = seq(as.Date("2026-01-04"), by = "week", length.out = 6),
  cases = c(10, 15, 24, 32, 21, 12)
)

plot <- islh_epi_curve(weekly, date = week, count = cases,
  title = "Weekly cases")
islh_plotly(plot)

islh_reactable(weekly, title = "Weekly counts", filterable = TRUE)
```

Put each widget in its own chunk, as the last expression. Both use Island
Health colours and BC Sans, work without a server, and keep their standard
Plotly or reactable interfaces for further customisation. In Quarto, set
`embed-resources: true` for a single shareable HTML file. For Word output,
print the original plot and use `islh_flextable()`.

Select and suppress values before creating a widget: the HTML includes its
underlying data, even when a column or hover field is hidden. The
[interactive reporting guide](https://temuulene.github.io/islandbrand/articles/interactive.html)
includes formatting, multiple-format reports, and a complete Quarto example.

## Epidemic curves

```r
library(islandbrand)

weekly_counts <- data.frame(
  period_start = seq(as.Date("2026-01-04"), by = "week", length.out = 8),
  count = c(1, 2, 4, 8, 6, 4, 2, 1)
)

islh_epi_curve(
  weekly_counts,
  date = period_start,
  count = count,
  title = "Reported cases by week"
)
```

Use `style = "cases"` to outline individual cases in a small outbreak. Routine
surveillance defaults to bars for faster rendering. Historical ranges can be
supplied as a separate reference data frame.

When starting from a line list:

```r
counts <- islandepi::islh_count_events(
  events,
  date = event_date,
  id = encounter_id,
  by = region,
  interval = "epiweek",
  fill = TRUE
)

islh_epi_curve(counts, period_start, count, fill = region)
```

## Using it in a report

```r
library(islandbrand)
islh_setup()
```

`islh_setup()` detects whether you are rendering to HTML or Word and configures
ggplot2 and the matching table engine. Run `islh_help()` for the functions you
will use most.

Start a complete project with:

```r
islandbrand::islh_create_report("2026-flu-season-report", format = "docx")
```

That writes a Quarto project with the Island Health format extension, brand
file and a worked example already wired up.

## Mapping BC health geographies

Use `islandepi` to retrieve standardized population and geography data, then
apply the map presentation layer from `islandbrand`:

```r
library(dplyr)
library(ggplot2)
library(islandepi)
library(islandbrand)

population <- islh_bc_population("lha", years = 2025, sex = "T") |>
  summarise(
    population = sum(population),
    .by = c(geography, geography_code, year, estimate_type)
  )

boundaries <- islh_bc_geography("lha")

map_data <- left_join(
  boundaries,
  population,
  by = join_by(geography, geography_code),
  relationship = "one-to-one",
  na_matches = "never"
)

if (anyNA(map_data$population)) {
  stop("One or more Island Health boundaries did not match population data.")
}

ggplot(map_data, aes(fill = population)) +
  geom_sf(colour = islh_hex("grey", 80), linewidth = 0.25) +
  scale_fill_islh_b(labels = scales::label_comma()) +
  labs(title = "Population by Local Health Area", fill = "Population") +
  theme_islh_map()
```

The population resource covers all of BC. The boundary helper defaults to
Island Health, so population rows from other health authorities are expected
to be discarded by this join.
