# What a draw is made of, kept with its sample: the output file says it, so that the
# sample can be reproduced.

#' Record the settings of a draw
#'
#' It is made when Sample is clicked, with the settings of that draw: the form can
#' change afterwards without a new Apply.
#'
#' @param params The settings of the draw, see `read_params()`.
#' @param file_name The name of the file of the sampling frame.
#' @param frame The sampling frame, as read from the file.
#' @param drawn_at The time of the draw.
#' @param info The version of the app, see `app_info()`.
#' @returns A list: the `params`, the `file_name`, the number of rows and of columns
#'   of the file (`n_rows`, `n_columns`), the time (`drawn_at`), and the version of
#'   the app (`app_version`) and of R (`r_version`).
#' @noRd
draw_record <- function(
  params,
  file_name,
  frame,
  drawn_at = Sys.time(),
  info = app_info()
) {
  list(
    params = params,
    file_name = file_name,
    n_rows = nrow(frame),
    n_columns = ncol(frame),
    drawn_at = drawn_at,
    app_version = info$label,
    r_version = R.version.string
  )
}

#' Name the output file of a draw
#'
#' The time of the draw, in UTC, so that a draw always gives the same name.
#'
#' @param record The record of the draw, see `draw_record()`.
#' @returns The file name, e.g. `PST_Sampling_Output_20261019-130531.xlsx`.
#' @noRd
output_file_name <- function(record) {
  paste0(
    "PST_Sampling_Output_",
    format(record$drawn_at, "%Y%m%d-%H%M%S", tz = "UTC"),
    ".xlsx"
  )
}
