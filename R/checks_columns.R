# Checks of the file and of the columns, run at Apply.

# Check that a stratification column has been selected when Stratified mode is active.
# Returns an error message string if the input is invalid, or NULL if valid.
validate_strata_selection <- function(input) {
  if (
    input$stratified == "Stratified" &&
      (is.null(input$strata) || input$strata == "None")
  ) {
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
validate_psu_column <- function(sframe, input) {
  if (input$samp_type != "Two-stage PPS cluster sampling with replacement") {
    return(NULL)
  }
  if (is.null(input$col_psu) || input$col_psu == "None") {
    return("Please select a cluster (PSU) column.")
  }
  if (input$stratified == "Stratified" && input$col_psu == input$strata) {
    return(
      "Cluster and stratification variables must be different columns."
    )
  }
  psu_col <- as.character(input$col_psu)
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
      input$col_psu,
      "' contains duplicate values: the sampling frame must have one row per cluster (PSU), so this column must uniquely identify each cluster."
    ))
  }
  return(NULL)
}

# Check that the selected population column is numeric when required for the sampling type.
# Returns an error message string if the input is invalid, or NULL if valid.
validate_population_column <- function(sframe, input) {
  if (
    !(input$samp_type %in%
      c(
        "Two-stage PPS cluster sampling with replacement",
        "PPS sampling with replacement"
      ))
  ) {
    return(NULL)
  }
  if (is.null(input$colpop) || input$colpop == "None") {
    return("Please select a population column for this sampling type.")
  }
  col <- as.character(input$colpop)
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
    input$samp_type == "Two-stage PPS cluster sampling with replacement" &&
      identical(col, as.character(input$col_psu))
  ) {
    return("Cluster and population variables must be different columns.")
  }
  if (
    input$stratified == "Stratified" &&
      identical(col, as.character(input$strata))
  ) {
    return("Stratification and population variables must be different columns.")
  }
  return(NULL)
}

# Rows whose population is missing or negative: a missing value would silently
# drop the row out of the frame and a negative one breaks the draw.
# Returns one row per problem (`data_row` ignores the header), or NULL.
population_issues <- function(sframe, input) {
  if (
    !(input$samp_type %in%
      c(
        "Two-stage PPS cluster sampling with replacement",
        "PPS sampling with replacement"
      ))
  ) {
    return(NULL)
  }
  values <- sframe[[as.character(input$colpop)]]
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
    psu = if (
      input$samp_type == "Two-stage PPS cluster sampling with replacement"
    ) {
      as.character(sframe[[as.character(input$col_psu)]][bad])
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
