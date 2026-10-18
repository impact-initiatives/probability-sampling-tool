# Checks of the file and of the columns, run at Apply.

# Check that a stratification column has been selected when Stratified mode is active.
# Returns an error message string if the input is invalid, or NULL if valid.
validate_strata_selection <- function(params) {
  if (params$stratified && is.null(params$strata_column)) {
    return(
      "Please select a stratification variable, or set 'Stratified ?' to 'Not stratified'."
    )
  }
  return(NULL)
}

# Check that the sampling frame has no column starting with the reserved prefix:
# it would be overwritten or break the draw. The usual cause is a previous
# sample reloaded as a sampling frame.
# Returns an error message string if one is present, or NULL if valid.
validate_reserved_columns <- function(sframe) {
  found <- names(sframe)[startsWith(names(sframe), RESERVED_PREFIX)]
  if (length(found) == 0) {
    return(NULL)
  }
  paste0(
    "The sampling frame has column(s) with protected names: ",
    paste(found, collapse = ", "),
    ".\n\nNames starting with ",
    RESERVED_PREFIX,
    " are reserved: the tool creates columns with these names during the sampling and in the sample file, so yours would be overwritten or cause an error. Rename or remove them in the CSV."
  )
}

# Check that the PSU (cluster) column is selected, distinct from the
# stratification column, and contains unique values (no duplicate PSU IDs).
# Returns an error message string if the input is invalid, or NULL if valid.
validate_psu_column <- function(sframe, params) {
  if (params$sampling_type != SAMPLING_TYPE_CLUSTER) {
    return(NULL)
  }
  if (is.null(params$psu_column)) {
    return("Please select a cluster (PSU) column.")
  }
  if (params$stratified && identical(params$psu_column, params$strata_column)) {
    return(
      "Cluster and stratification variables must be different columns."
    )
  }
  psu_col <- as.character(params$psu_column)
  if (is.null(sframe) || !psu_col %in% names(sframe)) {
    return(paste0(
      "Input cluster '",
      psu_col,
      "' was not found in the uploaded data. Please re-select the column."
    ))
  }
  if (anyDuplicated(sframe[[psu_col]]) > 0) {
    return(paste0(
      "Input cluster '",
      params$psu_column,
      "' contains duplicate values: the sampling frame must have one row per cluster (PSU), so this column must uniquely identify each cluster."
    ))
  }
  return(NULL)
}

# Check that the selected population column is numeric when required for the sampling type.
# Returns an error message string if the input is invalid, or NULL if valid.
validate_population_column <- function(sframe, params) {
  if (
    !(params$sampling_type %in%
      c(
        SAMPLING_TYPE_CLUSTER,
        SAMPLING_TYPE_PPS
      ))
  ) {
    return(NULL)
  }
  if (is.null(params$population_column)) {
    return("Please select a population column for this sampling type.")
  }
  col <- as.character(params$population_column)
  if (!(col %in% names(sframe))) {
    return(paste0(
      "Population column '",
      col,
      "' was not found in the uploaded dataset."
    ))
  }
  if (!is.numeric(sframe[[col]])) {
    return(paste0(
      "'",
      col,
      "' is not a numeric column. Select a numeric population column."
    ))
  }
  if (
    params$sampling_type == SAMPLING_TYPE_CLUSTER &&
      identical(col, as.character(params$psu_column))
  ) {
    return("Cluster and population variables must be different columns.")
  }
  if (params$stratified && identical(col, as.character(params$strata_column))) {
    return("Stratification and population variables must be different columns.")
  }
  return(NULL)
}

# Rows whose population is missing or negative: a missing value would silently
# drop the row out of the frame and a negative one breaks the draw.
# Returns one row per problem (`data_row` ignores the header), or NULL.
population_issues <- function(sframe, params) {
  if (
    !(params$sampling_type %in%
      c(
        SAMPLING_TYPE_CLUSTER,
        SAMPLING_TYPE_PPS
      ))
  ) {
    return(NULL)
  }
  values <- sframe[[as.character(params$population_column)]]
  bad <- which(is.na(values) | values < 0)
  if (length(bad) == 0) {
    return(NULL)
  }
  data.frame(
    issue = ifelse(
      is.na(values[bad]),
      "Missing population",
      "Negative population"
    ),
    data_row = bad,
    psu = if (params$sampling_type == SAMPLING_TYPE_CLUSTER) {
      as.character(sframe[[as.character(params$psu_column)]][bad])
    } else {
      NA_character_
    },
    population = values[bad]
  )
}


# Error message for the output of population_issues(), NULL when there is none.
population_message <- function(issues, col) {
  if (is.null(issues)) {
    return(NULL)
  }
  describe <- function(label, what) {
    rows <- issues$data_row[issues$issue == label]
    if (length(rows) > 0) {
      paste0(
        "Population column '",
        col,
        "' ",
        what,
        " in ",
        length(rows),
        " row(s): ",
        preview_ids(rows),
        " (header excluded)."
      )
    }
  }
  paste0(
    paste(
      c(
        describe("Missing population", "has missing values"),
        describe("Negative population", "has negative values")
      ),
      collapse = "\n"
    ),
    "\n\nThe population must be a number greater than or equal to 0: fix these values or remove the rows from the sampling frame."
  )
}
