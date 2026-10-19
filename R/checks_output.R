# The checks of the output file: the sample has to fit in an Excel sheet.

#' Check that the sample fits in a sheet of Excel
#'
#' The output file is an Excel workbook, and a sheet holds at most `EXCEL_MAX_ROWS`
#' rows (the header included) and `EXCEL_MAX_COLUMNS` columns.
#'
#' @param sample The drawn sample, the `sample` of `make_sample()`.
#' @returns An error message, `NULL` when the sample fits.
#' @noRd
excel_size_message <- function(sample) {
  if (nrow(sample) + 1 <= EXCEL_MAX_ROWS && ncol(sample) <= EXCEL_MAX_COLUMNS) {
    return(NULL)
  }
  check_message(
    "excel_limits",
    rows = format(nrow(sample), big.mark = ","),
    columns = format(ncol(sample), big.mark = ","),
    max_rows = format(EXCEL_MAX_ROWS, big.mark = ","),
    max_columns = format(EXCEL_MAX_COLUMNS, big.mark = ",")
  )
}
