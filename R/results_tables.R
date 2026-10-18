# The results as shown to the user: the tables, the alerts and the buttons.

#' Build the warning for a target above the population
#'
#' Shown when the requested sample size exceeds the population of some strata:
#' the sample is capped to the population there.
#'
#' @param affected The names of the strata concerned.
#' @param drawn `TRUE` once the sample has been drawn, `FALSE` at Apply.
#' @returns A `modalDialog`.
#' @noRd
exceeds_population_modal <- function(affected, drawn = FALSE) {
  modalDialog(
    title = tagList(
      icon("triangle-exclamation"),
      check_title("exceeds_population")
    ),
    div(
      class = "alert alert-warning",
      check_message(
        "exceeds_population",
        strata = paste(affected, collapse = ", "),
        tense = if (drawn) "was" else "will be"
      )
    ),
    easyClose = TRUE,
    footer = modalButton("OK")
  )
}

#' Get the body height of a table with one row per stratum
#'
#' The two small tables have one row per stratum (29px each, up to 5 rows, then
#' they scroll) and sit above the two large ones, which take what is left of the
#' page.
#'
#' @param data The table, with one row per stratum.
#' @returns A CSS length.
#' @noRd
small_table_height <- function(data) {
  paste0(min(nrow(data), 5) * 29 + 2, "px")
}
#' Get the body height of the large table of the Sampling frame tab
#'
#' It takes what is left of the page under the small table: the constant is the
#' rest of the tab (headers, filter rows and footers included), so the page fits
#' whatever the height of the window.
#'
#' @param small The height of the small table, as returned by `small_table_height()`.
#' @returns A CSS length.
#' @noRd
big_table_height <- function(small) {
  paste0("max(120px, 100vh - ", small, " - 520px)")
}
#' Get the body height of the large table of the Sampling tab
#'
#' Same idea as `big_table_height()`, with the rest of the Sampling tab.
#'
#' @param small The height of the small table, as returned by `small_table_height()`.
#' @returns A CSS length.
#' @noRd
sample_table_height <- function(small) {
  paste0("max(80px, 100vh - ", small, " - 520px)")
}

# Bootstrap 5 table look, with one line per cell and per header: a wide
# table scrolls sideways.
DT_STYLE <- "bootstrap5"
DT_CLASS <- "table table-sm table-striped table-hover nowrap"

#' Build a result table
#'
#' The four result tables share one look: Bootstrap 5, one line per cell and per
#' header (a wide table scrolls sideways), a filter per column and shorter numbers
#' for the columns the tool adds. These formats are display only: the data, and the
#' CSV files, are untouched.
#'
#' @param data The table to show.
#' @param height The height of the body of the table, a CSS length.
#' @param paging `TRUE` to page the table, `FALSE` to show every row.
#' @returns A DT table.
#' @noRd
result_table <- function(data, height, paging = FALSE) {
  table <- DT::datatable(
    data,
    rownames = FALSE,
    style = DT_STYLE,
    class = DT_CLASS,
    filter = "top",
    options = list(
      # table, then the number of entries and the pages on one line
      dom = if (paging) {
        "<'row'<'col-12'tr>><'row'<'col-md-5'i><'col-md-7'p>>"
      } else {
        "t"
      },
      paging = paging,
      pageLength = 100,
      scrollY = height,
      scrollCollapse = TRUE,
      scrollX = TRUE
    )
  )
  signif_cols <- intersect("PST_prob", names(data))
  round_cols <- intersect(c("DEFF", "ICC"), names(data))
  if (length(signif_cols) > 0) {
    table <- DT::formatSignif(table, signif_cols, digits = 4)
  }
  if (length(round_cols) > 0) {
    table <- DT::formatRound(table, round_cols, digits = 2)
  }
  table
}

#' Show a failed validation as an alert
#'
#' A failed validation is shown once, as an alert, instead of inside each table.
#'
#' @param expr An expression that may stop on a validation message.
#' @param extra Content added under the message, or `NULL`.
#' @returns An alert, or `NULL` when `expr` has no message.
#' @noRd
validation_alert <- function(expr, extra = NULL) {
  msg <- validation_message(expr)
  if (length(msg) && nzchar(msg)) {
    div(
      class = "alert alert-danger",
      span(style = "white-space: pre-line;", msg),
      extra
    )
  }
}

#' Build the link to download the list of the problems found
#'
#'
#'
#' @param id The id of the download output.
#' @returns A link, as HTML.
#' @noRd
issues_download_link <- function(id) {
  div(
    class = "mt-2",
    downloadLink(
      id,
      tagList(icon("download"), "Download the full list (CSV)")
    )
  )
}

#' Build an export button of the Sampling tab
#'
#' Real download buttons only exist once a sample has been drawn; before that, or
#' after a failed draw, inert placeholders are shown. `shinyjs::enable()` would
#' leave `aria-disabled` and `tabindex=-1` on the links.
#'
#' @param id The id of the download output.
#' @param label The text of the button.
#' @param style The inline CSS of the button.
#' @param ready `TRUE` once a sample has been drawn.
#' @returns A download button, or a disabled button.
#' @noRd
export_button <- function(id, label, style, ready) {
  if (ready) {
    downloadButton(
      id,
      label,
      class = "btn-outline-primary text-nowrap",
      style = style
    )
  } else {
    tags$button(
      type = "button",
      class = "btn btn-outline-primary text-nowrap",
      style = style,
      disabled = NA,
      icon("download"),
      label
    )
  }
}
