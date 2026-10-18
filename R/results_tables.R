# The results as shown to the user: the tables, the alerts and the buttons.

# Warning shown when the requested sample size exceeds the population of some
# strata (`affected`). `drawn` is TRUE once the sample has been drawn.
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

# Body height of the tables. The two small ones have one row per stratum
# (29px each, up to 5 rows, then they scroll) and sit above the two large
# ones, which take what is left of the page: the two constants are the rest
# of the Sampling frame tab and of the Sampling tab (headers, filter rows and
# footers included), so the page fits whatever the height.
small_table_height <- function(data) {
  paste0(min(nrow(data), 5) * 29 + 2, "px")
}
big_table_height <- function(small) {
  paste0("max(120px, 100vh - ", small, " - 520px)")
}
sample_table_height <- function(small) {
  paste0("max(80px, 100vh - ", small, " - 520px)")
}

# Bootstrap 5 table look, with one line per cell and per header: a wide
# table scrolls sideways.
DT_STYLE <- "bootstrap5"
DT_CLASS <- "table table-sm table-striped table-hover nowrap"

# The four result tables share one look: a filter per column and shorter
# numbers for the columns the tool adds. These formats are display only: the
# data, and the CSV files, are untouched.
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

# A failed validation is shown once, as an alert, instead of inside each table.
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

# A link to download the full list of the problems found, under an error message
issues_download_link <- function(id) {
  div(
    class = "mt-2",
    downloadLink(
      id,
      tagList(icon("download"), "Download the full list (CSV)")
    )
  )
}

# Real download buttons only exist once a sample has been drawn (`ready`); before
# that (or after a failed draw) inert placeholders are shown. shinyjs::enable()
# would leave aria-disabled and tabindex=-1 on the links.
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
