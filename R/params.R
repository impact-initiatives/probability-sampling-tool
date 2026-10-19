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

#' Read the settings of the sampling from the inputs of the interface
#'
#' The checks and the calculation take the list this returns, not the Shiny
#' inputs: they do not depend on the interface.
#'
#' @param input The Shiny `input` object.
#' @returns A named list with one element per field of `PARAM_FIELDS`, in their final
#'   shape (see `normalize_params()`).
#' @noRd
read_params <- function(input) {
  normalize_params(stats::setNames(
    lapply(PARAM_FIELDS, function(field) input[[field]]),
    PARAM_FIELDS
  ))
}

#' Give the settings their final shape
#'
#' `stratified` becomes `TRUE` or `FALSE` (anything but `TRUE` is `FALSE`), and a
#' column field becomes `NULL` when no column is chosen, the name of the column
#' otherwise.
#'
#' @param params The settings as the interface sends them, or as a test builds them.
#' @returns The same list, with `stratified` and the column fields normalised.
#' @noRd
normalize_params <- function(params) {
  params$stratified <- isTRUE(params$stratified)
  for (field in COLUMN_FIELDS) {
    params[field] <- list(column_choice(params[[field]]))
  }
  params
}

#' Read the column chosen in a select
#'
#' No choice is `NO_COLUMN` in the interface; a missing or empty value counts as
#' no choice too.
#'
#' @param choice The value of a column select.
#' @returns The name of the column, or `NULL` when none is chosen.
#' @noRd
column_choice <- function(choice) {
  if (length(choice) != 1 || is.na(choice) || !nzchar(choice)) {
    return(NULL)
  }
  if (identical(choice, NO_COLUMN)) {
    return(NULL)
  }
  choice
}
