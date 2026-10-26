# The output file: an Excel workbook with the inputs of the draw, the summary of the
# sample, the sample and the sampling frame it was drawn from.

# the sheets of the workbook, in this order; snake_case, no space and no digit
OUTPUT_SHEETS <- c(
  inputs = "inputs",
  summary = "sample_summary",
  sample = "sample",
  frame = "sampling_frame"
)

# the primary colour of the theme, for the headers of the inputs sheet
OUTPUT_HEADER_COLOUR <- "FF1A56A0"
# the light colour of the card headers, for the headers of the tables
OUTPUT_TABLE_HEADER_COLOUR <- "FFEEF3FA"

#' List the inputs of a draw
#'
#' What is needed to reproduce the sample, in sections: the traceability (version
#' of the app and of R, time of the draw), the data source, the sampling and the
#' parameters. Only the settings that the mode and the type of the draw use are
#' listed: the ICC is listed for the cluster type in both modes (the design effect
#' is shown in both), the buffer and the confidence level only when the size is
#' computed from the population, the target only when it is entered.
#'
#' @param record The record of the draw, see `draw_record()`.
#' @returns A list with one element per line: a list with its `section`, its
#'   `setting` and its `value` (a number stays a number).
#' @noRd
input_rows <- function(record) {
  p <- record$params
  label <- function(field) unname(FIELD_LABELS[[field]])
  line <- function(section, setting, value) {
    list(section = section, setting = setting, value = value)
  }
  by_population <- p$sampling_mode == SAMPLING_MODE_FROM_POPULATION
  cluster <- p$sampling_type == SAMPLING_TYPE_CLUSTER

  rows <- list(
    line("Traceability", "App version", record$app_version),
    line("Traceability", "R version", record$r_version),
    line(
      "Traceability",
      "Date and time of the draw (UTC)",
      format(record$drawn_at, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ),
    line("Data source", "File", record$file_name),
    line("Data source", "Rows", record$n_rows),
    line("Data source", "Columns", record$n_columns),
    line("Sampling", label("sampling_mode"), p$sampling_mode),
    line("Sampling", label("sampling_type"), p$sampling_type),
    line("Sampling", label("stratified"), if (p$stratified) "Yes" else "No")
  )
  if (p$stratified) {
    rows <- c(
      rows,
      list(line("Sampling", label("strata_column"), p$strata_column))
    )
  }
  if (cluster) {
    rows <- c(rows, list(line("Sampling", label("psu_column"), p$psu_column)))
  }
  if (p$sampling_type != SAMPLING_TYPE_SIMPLE) {
    rows <- c(
      rows,
      list(line("Sampling", label("population_column"), p$population_column))
    )
  }

  parameter <- function(field, value = p[[field]]) {
    list(line("Parameters", label(field), value))
  }
  if (by_population) {
    rows <- c(
      rows,
      parameter("confidence_level"),
      parameter("error_margin"),
      parameter("proportion", FIXED_PROPORTION),
      parameter("buffer")
    )
  } else {
    rows <- c(rows, parameter("target_size"))
  }
  if (cluster) {
    rows <- c(rows, parameter("cluster_size"), parameter("icc"))
  }
  c(rows, parameter("seed", as.integer(p$seed)))
}

#' Build the output workbook of a draw
#'
#' Four sheets, see `OUTPUT_SHEETS`: the inputs of the draw, the summary of the
#' sample, the sample and the sampling frame. The summary and the sample have the
#' content of the tables of the app; the frame is the one the tool read, which the
#' record keeps, so that the draw can be reproduced from the file.
#'
#' @param sample The drawn sample, the `sample` of `make_sample()`.
#' @param summary The summary of the sample, the `summary_sample` of
#'   `make_sample()`.
#' @param record The record of the draw, see `draw_record()`, with its `frame`.
#' @returns A workbook, to save with its `save()` method.
#' @noRd
build_output_workbook <- function(sample, summary, record) {
  wb <- openxlsx2::wb_workbook(
    creator = "Probability sampling tool",
    title = "Probability sampling tool: sampling output"
  )
  add_inputs_sheet(wb, record)
  add_table_sheet(wb, OUTPUT_SHEETS[["summary"]], summary)
  add_table_sheet(wb, OUTPUT_SHEETS[["sample"]], sample)
  add_table_sheet(wb, OUTPUT_SHEETS[["frame"]], record$frame)
  wb
}

#' Add the inputs sheet to a workbook
#'
#' A title, then the sections, each with its header and its lines, and a sentence
#' that says what the inputs are for. The columns are as wide as their content.
#'
#' @param wb The workbook, which is changed in place.
#' @param record The record of the draw, see `draw_record()`.
#' @returns The workbook, invisibly.
#' @noRd
add_inputs_sheet <- function(wb, record) {
  sheet <- OUTPUT_SHEETS[["inputs"]]
  wb$add_worksheet(sheet, grid_lines = FALSE)
  rows <- input_rows(record)

  wb$add_data(
    sheet,
    "Probability sampling tool: inputs of the draw",
    dims = "A1"
  )
  wb$add_font(sheet, dims = "A1", bold = TRUE, size = 14)

  row_number <- 3
  current_section <- ""
  for (line in rows) {
    if (line$section != current_section) {
      # a blank line between two sections
      if (row_number > 3) {
        row_number <- row_number + 1
      }
      header <- openxlsx2::wb_dims(rows = row_number, cols = 1:2)
      wb$add_data(sheet, line$section, dims = paste0("A", row_number))
      wb$add_fill(
        sheet,
        dims = header,
        color = openxlsx2::wb_color(hex = OUTPUT_HEADER_COLOUR)
      )
      wb$add_font(
        sheet,
        dims = header,
        bold = TRUE,
        color = openxlsx2::wb_color(hex = "FFFFFFFF")
      )
      current_section <- line$section
      row_number <- row_number + 1
    }
    wb$add_data(sheet, line$setting, dims = paste0("A", row_number))
    wb$add_data(sheet, line$value, dims = paste0("B", row_number))
    wb$add_cell_style(
      sheet,
      dims = paste0("B", row_number),
      horizontal = "left",
      vertical = "top",
      wrap_text = TRUE
    )
    row_number <- row_number + 1
  }

  note_row <- row_number + 1
  note <- "The same file, the same settings, the same seed and the same version of the app give the same sample."
  wb$add_data(sheet, note, dims = paste0("A", note_row))
  wb$merge_cells(sheet, dims = openxlsx2::wb_dims(rows = note_row, cols = 1:2))
  wb$add_font(sheet, dims = paste0("A", note_row), italic = TRUE)
  wb$add_cell_style(
    sheet,
    dims = paste0("A", note_row),
    wrap_text = TRUE,
    vertical = "top"
  )
  wb$set_row_heights(sheet, rows = note_row, heights = 32)

  settings <- c(
    vapply(rows, function(line) line$setting, character(1)),
    vapply(rows, function(line) line$section, character(1))
  )
  values <- vapply(rows, function(line) as.character(line$value), character(1))
  wb$set_col_widths(
    sheet,
    cols = 1:2,
    widths = c(
      max(28, max(nchar(settings)) + 4),
      min(80, max(30, max(nchar(values)) + 4))
    )
  )
  invisible(wb)
}

#' Add a table to a workbook as a sheet
#'
#' The first row is the header, frozen and with a filter, and the columns are as wide
#' as their header and their first values. An `NA` is an empty cell.
#'
#' @param wb The workbook, which is changed in place.
#' @param sheet The name of the sheet.
#' @param data The table, a data frame.
#' @returns The workbook, invisibly.
#' @noRd
add_table_sheet <- function(wb, sheet, data) {
  wb$add_worksheet(sheet)
  wb$add_data(sheet, as.data.frame(data), na.strings = "")
  if (ncol(data) > 0) {
    header <- openxlsx2::wb_dims(rows = 1, cols = seq_len(ncol(data)))
    wb$add_fill(
      sheet,
      dims = header,
      color = openxlsx2::wb_color(hex = OUTPUT_TABLE_HEADER_COLOUR)
    )
    wb$add_font(sheet, dims = header, bold = TRUE)
    wb$add_border(sheet, dims = header, bottom_border = "thin")
    wb$freeze_pane(sheet, first_row = TRUE)
    wb$add_filter(sheet, rows = 1, cols = seq_len(ncol(data)))
    wb$set_col_widths(
      sheet,
      cols = seq_len(ncol(data)),
      widths = table_column_widths(data)
    )
  }
  invisible(wb)
}

#' Choose the width of the columns of a table
#'
#' From the header (with room for the filter button) and the first values: reading
#' every value of a large sample would take long for no better result.
#'
#' @param data The table, a data frame.
#' @param rows The number of rows to read.
#' @returns A number per column, between 8 and 45 characters.
#' @noRd
table_column_widths <- function(data, rows = 200) {
  head_rows <- utils::head(as.data.frame(data), rows)
  widths <- vapply(
    seq_len(ncol(data)),
    function(i) {
      values <- as.character(head_rows[[i]])
      longest <- max(nchar(values[!is.na(values)]), 0)
      max(nchar(names(data)[i]) + 4, longest + 2)
    },
    numeric(1)
  )
  pmin(pmax(widths, 8), 45)
}
