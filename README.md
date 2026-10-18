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

On Windows, restart R, then run:

```r
install.packages("remotes", type = "binary")
remotes::install_github(
  "temuulene/islandbrand",
  type = "binary", upgrade = "always", build = FALSE
)
islandbrand::islh_install_deps("html")   # or "docx", or "both"
```

This installs the latest version from GitHub. Its CRAN packages arrive as
ready-made Windows binaries and `islandbrand` has no code to compile, so you
do not need Rtools, Git, a GitHub account or a token. Run the same commands
again to update. For interactive HTML reports, use
`islh_install_deps("html", interactive = TRUE)`.

To install `islandepi` at the same time, give both names:

```r
remotes::install_github(
  c("temuulene/islandepi", "temuulene/islandbrand"),
  type = "binary", upgrade = "always", build = FALSE
)
```

If GitHub answers "API rate limit exceeded", many people on the same network
have installed within the hour. Wait an hour, or use a release file below.

### A specific version, or no access to GitHub

To install a version your team has approved, add its tag from the
[Releases page](https://github.com/temuulene/islandbrand/releases) to the
name, such as `"temuulene/islandbrand@vX.Y.Z"`.

Without GitHub access, get the Windows `.zip` for the approved version from
your team lead or the Releases page. Install the packages `islandbrand` is
built on first, then the file:

```r
install.packages(
  c("cli", "ggplot2", "rlang", "scales", "systemfonts"),
  type = "binary"
)
install.packages("path/to/islandbrand_x.y.z.zip", repos = NULL)
islandbrand::islh_install_deps("html")   # or "docx", or "both"
```

PHASE staff can instead use `install-phase.R` from the paired
[islandepi release](https://github.com/temuulene/islandepi/releases), which
also checks the version and confirms the package loads.

Record both package versions and `sessionInfo()` with each report. To render
Quarto reports, install Quarto separately.

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
