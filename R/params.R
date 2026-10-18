# The settings of the sampling, as one plain list.

# The fields of the list, named like the inputs of the interface they come from
PARAM_FIELDS <- c(
  "sampling_mode",
  "sampling_type",
  "stratified",
  "strata_column",
  "psu_column",
  "population_column",
  "confidence_level",
  "error_margin",
  "buffer",
  "target_size",
  "cluster_size",
  "icc",
  "seed"
)

# The fields that hold the name of a column of the sampling frame
COLUMN_FIELDS <- c("strata_column", "psu_column", "population_column")

# Reads the settings of the sampling from the inputs of the interface. The
# checks and the calculation take this list, not the Shiny inputs: they do not
# depend on the interface.
read_params <- function(input) {
  normalize_params(stats::setNames(
    lapply(PARAM_FIELDS, function(field) input[[field]]),
    PARAM_FIELDS
  ))
}

# Gives the settings their final shape: `stratified` is TRUE or FALSE, and a
# column field is NULL when no column is chosen, otherwise the name of the column.
normalize_params <- function(params) {
  params$stratified <- identical(params$stratified, STRATIFIED)
  for (field in COLUMN_FIELDS) {
    params[field] <- list(column_choice(params[[field]]))
  }
  params
}

# The column chosen in a select: NULL for no choice, which the interface shows as
# NO_COLUMN, and also for a missing or empty value.
column_choice <- function(choice) {
  if (length(choice) != 1 || is.na(choice) || !nzchar(choice)) {
    return(NULL)
  }
  if (identical(choice, NO_COLUMN)) {
    return(NULL)
  }
  choice
}
