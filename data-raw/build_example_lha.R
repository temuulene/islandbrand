# Builds inst/extdata/islh-lha.gpkg, the example local health area map used by
# islh_example_lha(), the article examples and the tests.
#
# The package ships this so every map example runs offline. Staff analyses
# should still retrieve current boundaries and denominators with islandepi's
# islh_bc_geography() and islh_bc_population().
#
# Sources, both under the Open Government Licence - British Columbia:
#
# * Local Health Area Boundaries, BC Data Catalogue record
#   afd021d9-7722-4410-b506-d394c66e74fc (WFS layer
#   WHSE_ADMIN_BOUNDARIES.BCHA_LOCAL_HEALTH_AREA_SP). The polygons already
#   follow the coastline.
# * BC Sub-Provincial Population Estimates and Projections, record
#   86839277-986a-4a29-9f70-fa9b1166f6cb, resource
#   d4bbb2a0-aff7-403f-b52a-a634d05ee70f ("Local Health Areas").
#
# Run with: Rscript data-raw/build_example_lha.R
# Needs sf and rmapshaper, and network access to the two services.
#
# The services change: boundaries are corrected and population estimates are
# revised. So the script also writes inst/extdata/islh-lha-provenance.csv, a
# build record of when the data was retrieved, from where, and the MD5 hash of
# each raw response, so a later rebuild can be told apart from this one.

year <- 2025L
keep <- 0.02

boundary_url <- paste0(
  "https://openmaps.gov.bc.ca/geo/pub/wfs?service=WFS&version=2.0.0",
  "&request=GetFeature",
  "&typeName=pub:WHSE_ADMIN_BOUNDARIES.BCHA_LOCAL_HEALTH_AREA_SP",
  "&outputFormat=json",
  "&propertyName=LOCAL_HLTH_AREA_CODE,LOCAL_HLTH_AREA_NAME,SHAPE",
  "&CQL_FILTER=HLTH_AUTHORITY_NAME=%27Vancouver%20Island%27"
)
population_url <- paste0(
  "https://catalogue.data.gov.bc.ca/dataset/",
  "86839277-986a-4a29-9f70-fa9b1166f6cb/resource/",
  "d4bbb2a0-aff7-403f-b52a-a634d05ee70f/download/",
  "local-health-area-population.csv"
)

# Save each raw response before reading it, so its hash records exactly what
# the service returned.
retrieved <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
boundary_file <- tempfile(fileext = ".json")
population_file <- tempfile(fileext = ".csv")
utils::download.file(boundary_url, boundary_file, mode = "wb", quiet = TRUE)
utils::download.file(population_url, population_file, mode = "wb", quiet = TRUE)

boundaries <- sf::st_read(boundary_file, quiet = TRUE)
boundaries <- sf::st_transform(boundaries, 3005)
boundaries <- sf::st_make_valid(boundaries)
stopifnot(nrow(boundaries) == 14L)

# ms_simplify() moves shared borders together, so neighbouring areas do not
# open slivers between them, and keep_shapes keeps the small islands.
boundaries <- rmapshaper::ms_simplify(
  boundaries,
  keep = keep,
  keep_shapes = TRUE
)
boundaries <- sf::st_make_valid(boundaries)

population <- utils::read.csv(population_file, check.names = FALSE)
population <- population[
  population$Year == year &
    population$Gender == "T" &
    population$Type == "Estimate",
  c("Region", "Total")
]
population$Region <- sprintf("%03d", as.integer(population$Region))

lha <- data.frame(
  geography_code = as.character(boundaries$LOCAL_HLTH_AREA_CODE),
  geography_name = boundaries$LOCAL_HLTH_AREA_NAME,
  stringsAsFactors = FALSE
)
hsda_names <- c(
  "41" = "South Vancouver Island",
  "42" = "Central Vancouver Island",
  "43" = "North Vancouver Island"
)
lha$hsda <- unname(hsda_names[substr(lha$geography_code, 1, 2)])
lha$population <- population$Total[match(lha$geography_code, population$Region)]
lha$year <- year
stopifnot(!anyNA(lha$population), !anyNA(lha$hsda))

out <- sf::st_sf(lha, geometry = sf::st_geometry(boundaries))
out <- out[order(out$geography_code), ]
rownames(out) <- NULL

path <- "inst/extdata/islh-lha.gpkg"
unlink(path)
sf::st_write(out, path, layer = "lha", quiet = TRUE)
cat("wrote", path, format(file.size(path), big.mark = ","), "bytes\n")

md5 <- function(file) unname(tools::md5sum(file))
provenance <- data.frame(
  field = c(
    "dataset",
    "retrieved_utc",
    "boundary_source",
    "boundary_layer",
    "boundary_url",
    "boundary_md5",
    "boundary_bytes",
    "population_source",
    "population_url",
    "population_md5",
    "population_bytes",
    "population_year",
    "population_rows",
    "crs",
    "simplification",
    "licence",
    "software",
    "note"
  ),
  value = c(
    path,
    retrieved,
    "BC Data Catalogue record afd021d9-7722-4410-b506-d394c66e74fc",
    "WHSE_ADMIN_BOUNDARIES.BCHA_LOCAL_HEALTH_AREA_SP",
    boundary_url,
    md5(boundary_file),
    file.size(boundary_file),
    paste(
      "BC Data Catalogue record 86839277-986a-4a29-9f70-fa9b1166f6cb,",
      "resource d4bbb2a0-aff7-403f-b52a-a634d05ee70f"
    ),
    population_url,
    md5(population_file),
    file.size(population_file),
    year,
    "Gender T, Type Estimate",
    "EPSG:3005",
    paste0("rmapshaper::ms_simplify(keep = ", keep, ", keep_shapes = TRUE)"),
    "Open Government Licence - British Columbia",
    paste0(
      R.version.string,
      "; sf ",
      utils::packageVersion("sf"),
      "; rmapshaper ",
      utils::packageVersion("rmapshaper")
    ),
    "Built by data-raw/build_example_lha.R."
  )
)
record <- "inst/extdata/islh-lha-provenance.csv"
utils::write.csv(provenance, record, row.names = FALSE)
cat("wrote", record, "\n")
