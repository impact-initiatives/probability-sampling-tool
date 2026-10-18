# Checks of the file and of the columns, run at Apply.

# Check that a stratification column has been selected when Stratified mode is active.
# Returns an error message string if the input is invalid, or NULL if valid.
validate_strata_selection <- function(params) {
  if (params$stratified && is.null(params$strata_column)) {
    return(check_message("strata_missing"))
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
  check_message(
    "reserved_columns",
    columns = paste(found, collapse = ", "),
    prefix = RESERVED_PREFIX
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
    return(check_message("psu_missing"))
  }
  if (params$stratified && identical(params$psu_column, params$strata_column)) {
    return(check_message("cluster_strata_same"))
  }
  psu_col <- as.character(params$psu_column)
  if (is.null(sframe) || !psu_col %in% names(sframe)) {
    return(check_message("psu_not_found", column = psu_col))
  }
  if (anyDuplicated(sframe[[psu_col]]) > 0) {
    return(check_message("psu_duplicates", column = params$psu_column))
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
    return(check_message("population_missing"))
  }
  col <- as.character(params$population_column)
  if (!(col %in% names(sframe))) {
    return(check_message("population_not_found", column = col))
  }
  if (!is.numeric(sframe[[col]])) {
    return(check_message("population_not_numeric", column = col))
  }
  if (
    params$sampling_type == SAMPLING_TYPE_CLUSTER &&
      identical(col, as.character(params$psu_column))
  ) {
    return(check_message("cluster_population_same"))
  }
  if (params$stratified && identical(col, as.character(params$strata_column))) {
    return(check_message("strata_population_same"))
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
  describe <- function(label, id) {
    rows <- issues$data_row[issues$issue == label]
    if (length(rows) > 0) {
      check_message(
        id,
        column = col,
        n = length(rows),
        rows = preview_ids(rows)
      )
    }
  }
  paste0(
    paste(
      c(
        describe("Missing population", "population_missing_values"),
        describe("Negative population", "population_negative_values")
      ),
      collapse = "\n"
    ),
    "\n\n",
    check_text("population_values_advice")
  )
}
