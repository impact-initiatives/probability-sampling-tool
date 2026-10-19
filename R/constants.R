# Values shared by the interface and the code. Constants are written in upper
# case. The choices of the interface are defined here once: the interface, the
# checks and the draw all use these values, so a name is changed in one place.

# The version of the app, in calendar versioning like the branches and the
# milestone. A release tag vX.Y.Z must be equal to it (checked by the CI, see #56).
APP_VERSION <- "2026.1.0"

# What one row of the sampling frame is, for each type (simple, pps and cluster):
# the short form for the tables that compare the types, the longer
# one for the page of the type. The Guide reads them ({{one_row:pps}},
# {{one_row_detail:pps}}); the README repeats the short ones, a test checks them.
ONE_ROW <- c(
  simple = "a unit to survey (household, person)",
  pps = "a PSU (an area or a population group) with its population",
  cluster = "a PSU (hexagon, village, camp...) with its population"
)
ONE_ROW_DETAIL <- c(
  simple = "a unit to survey: a household, a person, a beneficiary",
  pps = "a PSU: an area (region, camp, settlement) or any population group (host, IDP, returnee), with its population",
  cluster = "a PSU (hexagon GRID_ID, village, camp...) with its population"
)

# the three sampling types
SAMPLING_TYPE_SIMPLE <- "Simple random sampling"
SAMPLING_TYPE_PPS <- "PPS sampling with replacement"
SAMPLING_TYPE_CLUSTER <- "Two-stage PPS cluster sampling with replacement"

# how the sample size is set
SAMPLING_MODE_ENTER_SIZE <- "Enter sample size"
SAMPLING_MODE_FROM_POPULATION <- "Sample size based on population"


# where the sampling frame comes from
DATA_SOURCE_UPLOAD <- "upload"
DATA_SOURCE_TEST <- "test"

# the columns of the example data that each column field selects when the
# example data is used, named by the field they fill
EXAMPLE_COLUMNS <- c(
  strata_column = "strata",
  psu_column = "psu_id",
  population_column = "population"
)

# the example data, bundled with the app
EXAMPLE_DATA_PATH <- "data/data_example.csv"

# the output file: what a sheet of Excel holds at most
EXCEL_MAX_ROWS <- 1048576
EXCEL_MAX_COLUMNS <- 16384

# the choice of a column select that stands for "no column"
NO_COLUMN <- "None"

# The largest buffer, a share of the target: 2 is 200%. A buffer above it is
# most likely a number of surveys typed in place of a share.
MAX_BUFFER <- 2

# Proportion used by the sample size computation. It is fixed: 0.5 is the most
# cautious value, since p * (1 - p) is largest there and so is the sample size.
# The field stays visible, not editable, in the interface (issues #8 and #9).
FIXED_PROPORTION <- 0.5

# The labels of the fields of the form, by the name of the field in the settings
# (see PARAM_FIELDS). The form, the Guide and the summary all read them here.
FIELD_LABELS <- c(
  sampling_mode = "Mode of sampling",
  sampling_type = "Type of sampling",
  stratified = "Stratified",
  strata_column = "Input strata",
  psu_column = "Input cluster",
  population_column = "Input population",
  confidence_level = "Confidence level",
  error_margin = "Error margin",
  proportion = "Proportion",
  buffer = "Buffer",
  target_size = "Enter target sample size",
  cluster_size = "Cluster size",
  icc = "ICC",
  seed = "Seed"
)

# Order of the columns of the tables and of the CSV files. The columns the tool
# adds come first, by order of importance, then the columns of the sampling
# frame in their own order. A name that is not in the table is ignored, so one
# order serves every sampling type and mode.
FRAME_COLUMN_ORDER <- c(
  "PST_id_sample",
  "PST_stratum_id",
  "PST_psu_id",
  "PST_pop_size",
  "PST_pop_size_stratum",
  "PST_prob"
)
SAMPLE_COLUMN_ORDER <- c(
  "PST_id_sample",
  "PST_survey",
  "PST_stratum_id",
  "PST_psu_id",
  "PST_pop_size",
  "PST_pop_size_stratum",
  "PST_prob"
)
# The labels shown to the user: label = name of the column in the code. The
# code names its columns in snake_case; only these tables know the labels.
TARGET_LABELS <- c(
  "Population" = "population",
  "Requested Target" = "target",
  "Requested Target (with buffer)" = "target_with_buffer"
)
SUMMARY_LABELS <- c(
  "Stratum" = "PST_stratum_id",
  "Surveys" = "surveys",
  "Clusters drawn" = "clusters_drawn",
  "Distinct PSUs" = "distinct_psus",
  "Population" = "population",
  "Requested Target (with buffer)" = "target_with_buffer",
  "DEFF" = "deff",
  "Effective sample size" = "effective_sample_size",
  "Sampling type" = "sampling_type",
  # the parameters are labelled like their field of the form
  stats::setNames(
    c(
      "cluster_size",
      "icc",
      "buffer",
      "confidence_level",
      "error_margin",
      "seed"
    ),
    FIELD_LABELS[c(
      "cluster_size",
      "icc",
      "buffer",
      "confidence_level",
      "error_margin",
      "seed"
    )]
  )
)
# every label of a column, by the name the code gives the column
COLUMN_LABELS <- c(TARGET_LABELS, SUMMARY_LABELS)

# the summary: stratum and population, the result, the draw, the design, the
# parameters, then the seed and the sampling type
SUMMARY_COLUMN_ORDER <- c(
  "Stratum",
  "Population",
  "Surveys",
  "Requested Target (with buffer)",
  "Clusters drawn",
  "Distinct PSUs",
  unname(FIELD_LABELS["cluster_size"]),
  "DEFF",
  "Effective sample size",
  unname(FIELD_LABELS[c(
    "icc",
    "buffer",
    "confidence_level",
    "error_margin",
    "seed"
  )]),
  "Sampling type"
)

# Prefix of the columns the tool adds to the sampling frame and to the sample.
RESERVED_PREFIX <- "PST_"
