# Values shared by the interface and the code: the proportion of the sample
# size, the order of the columns of the tables and the reserved prefix.

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
