# Reading of the uploaded file.

# Message when `name` is not a .csv file, NULL otherwise.
csv_format_error <- function(name) {
  if (tolower(tools::file_ext(name)) != "csv") {
    paste0(
      "'",
      name,
      "' is not a CSV file. Only .csv files are accepted: for an ",
      "Excel workbook, save the sheet you need as CSV first."
    )
  }
}

# Reads an uploaded file as a CSV table.
# Returns list(data, error): `data` is NULL and `error` holds a message when the
# file is not a .csv or cannot be read as a table.
read_uploaded_csv <- function(name, path) {
  fail <- function(msg) list(data = NULL, error = msg)

  format_error <- csv_format_error(name)
  if (!is.null(format_error)) {
    return(fail(format_error))
  }

  # a parse warning means rows were dropped (e.g. a ragged line): reject the file.
  # Raising an error from the warning handler would leave fread() in a bad state.
  parse_warned <- FALSE
  data <- tryCatch(
    withCallingHandlers(
      data.table::fread(path),
      warning = function(w) {
        parse_warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  if (parse_warned || is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(fail(paste0(
      "'",
      name,
      "' could not be read as a CSV table (empty or damaged file)."
    )))
  }
  list(data = data, error = NULL)
}
