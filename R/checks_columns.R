# Checks of the file and of the columns, run at Apply.

#' Check that a stratification column is chosen
#'
#' When the sample is stratified, a column must say which stratum each unit
#' belongs to.
#'
#' @param params The settings of the sampling, see `read_params()`.
#' @returns An error message when the sample is stratified and no column is chosen,
#'   `NULL` otherwise.
#' @noRd
validate_strata_selection <- function(params) {
  if (params$stratified && is.null(params$strata_column)) {
    return(check_message("strata_missing"))
  }
  return(NULL)
}

#' Check that the sampling frame has no column with a reserved name
#'
#' A column whose name starts with `RESERVED_PREFIX` would be overwritten or
#' break the draw. The usual cause is a previous sample reloaded as a sampling
#' frame.
#'
#' @param sframe The sampling frame, as read from the uploaded file.
#' @returns An error message that lists the columns concerned, `NULL` when there is none.
#' @noRd
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

#' Check that the stratification column is in the sampling frame
#'
#' The selection can be older than the file when the file has been changed.
#' Nothing is checked when the sample is not stratified, or when no column is
#' chosen: `validate_strata_selection()` reports that case.
#'
#' @param sframe The sampling frame, as read from the uploaded file.
#' @param params The settings of the sampling, see `read_params()`.
#' @returns An error message when the column is not in the frame, `NULL` otherwise.
#' @noRd
validate_strata_column <- function(sframe, params) {
  if (!params$stratified || is.null(params$strata_column)) {
    return(NULL)
  }
  column <- as.character(params$strata_column)
  if (!column %in% names(sframe)) {
    return(check_message("strata_not_found", column = column))
  }
  return(NULL)
}

#' Check the column that identifies the clusters
#'
#' For the cluster type, the PSU column must be chosen, differ from the
#' stratification column, be in the frame and identify each PSU once. The other
#' types do not use it.
#'
#' @param sframe The sampling frame, as read from the uploaded file.
#' @param params The settings of the sampling, see `read_params()`.
#' @returns An error message for the first problem found, `NULL` when the column is
#'   valid or not used.
#' @noRd
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

#' Check the population column
#'
#' For the PPS and cluster types, the population column must be chosen, be in the
#' frame, be numeric and differ from the PSU and stratification columns. The
#' other types do not use it.
#'
#' @param sframe The sampling frame, as read from the uploaded file.
#' @param params The settings of the sampling, see `read_params()`.
#' @returns An error message for the first problem found, `NULL` when the column is
#'   valid or not used.
#' @noRd
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

#' Find the rows whose population is missing or negative
#'
#' A missing value would silently drop the row out of the frame and a negative
#' one breaks the draw. Only the types that use a population column are
#' concerned.
#'
#' @param sframe The sampling frame, as read from the uploaded file.
#' @param params The settings of the sampling, see `read_params()`.
#' @returns A data frame with one row per problem (`issue`, `data_row`, `psu`,
#'   `population`), `NULL` when there is none. `data_row` is the position in the
#'   file, header excluded.
#' @noRd
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


#' Build the error message for the rows with a bad population
#'
#' One line per kind of problem, with a few of the rows, then what to do.
#'
#' @param issues The output of `population_issues()`.
#' @param col The name of the population column.
#' @returns An error message, `NULL` when `issues` is `NULL`.
#' @noRd
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
