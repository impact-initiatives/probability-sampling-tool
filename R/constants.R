# Values shared by the interface and the code. Constants are written in upper
# case. The choices of the interface are defined here once: the interface, the
# checks and the draw all use these values, so a name is changed in one place.

# the three sampling types
SAMPLING_TYPE_SIMPLE <- "Simple random sampling"
SAMPLING_TYPE_PPS <- "PPS sampling with replacement"
SAMPLING_TYPE_CLUSTER <- "Two-stage PPS cluster sampling with replacement"

# how the sample size is set
SAMPLING_MODE_ENTER_SIZE <- "Enter sample size"
SAMPLING_MODE_FROM_POPULATION <- "Sample size based on population"

# stratified or not
STRATIFIED <- "Stratified"
NOT_STRATIFIED <- "Not stratified"

# where the sampling frame comes from
DATA_SOURCE_UPLOAD <- "upload"
DATA_SOURCE_TEST <- "test"

# the choice of a column select that stands for "no column"
NO_COLUMN <- "None"

# Proportion used by the sample size computation. It is fixed: 0.5 is the most
# cautious value, since p * (1 - p) is largest there and so is the sample size.
# The field stays visible, not editable, in the interface (issues #8 and #9).
FIXED_PROPORTION <- 0.5

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
  "Cluster size" = "cluster_size",
  "ICC" = "icc",
  "DEFF" = "deff",
  "Effective sample size" = "effective_sample_size",
  "Buffer" = "buffer",
  "Confidence level" = "confidence_level",
  "Error margin" = "error_margin",
  "Sampling type" = "sampling_type",
  "Seed" = "seed"
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
  "Cluster size",
  "DEFF",
  "Effective sample size",
  "ICC",
  "Buffer",
  "Confidence level",
  "Error margin",
  "Seed",
  "Sampling type"
)

# Prefix of the columns the tool adds to the sampling frame and to the sample.
RESERVED_PREFIX <- "PST_"
