# Reading of the uploaded file.

#' Check that a file name ends in .csv
#'
#'
#'
#' @param name The name of the uploaded file.
#' @returns An error message when the name is not a .csv file, `NULL` otherwise.
#' @noRd
csv_format_error <- function(name) {
  if (tolower(tools::file_ext(name)) != "csv") {
    check_message("csv_format", name = name)
  }
}

#' Read an uploaded file as a CSV table
#'
#' The file is refused if it is not a .csv, if it cannot be read as a table or if
#' reading it gave a warning, which means rows were dropped.
#'
#' @param name The name of the uploaded file.
#' @param path The path of the uploaded file on the server.
#' @returns A list with `data` (the table, or `NULL`) and `error` (a message, or `NULL`).
#' @noRd
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
    return(fail(check_message("csv_unreadable", name = name)))
  }
  list(data = data, error = NULL)
}
