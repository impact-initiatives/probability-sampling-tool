# The "Sampling" tab: the draw, the exports and the two tables of the result.

#' Build the Sampling tab
#'
#' The draw and the exports in a sidebar, and the two tables of the result: the
#' summary and the sample.
#'
#' @returns A `nav_panel`.
#' @noRd
sampling_tab <- function() {
  nav_panel(
    "Sampling",
    layout_sidebar(
      fill = FALSE,
      fillable = FALSE,
      padding = "1rem",
      sidebar = tab_sidebar(
        id = "sampling_sidebar",
        width = 300,
        sidebar_header("Actions"),
        div(
          class = "p-3",
          shinyjs::disabled(
            actionButton(
              "draw_button",
              "Sample",
              class = "btn-primary",
              width = "100%"
            )
          ),
          uiOutput("sample_status")
        ),
        sidebar_header("Export"),
        div(class = "p-3", uiOutput("export_buttons"))
      ),
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        fill = FALSE,
        table_card(
          "Sample summary",
          uiOutput("sample_error"),
          uiOutput("summary_empty"),
          DT::dataTableOutput("summary_table")
        ),
        table_card(
          "Sample",
          uiOutput("sample_empty"),
          DT::dataTableOutput("sample_table")
        )
      )
    )
  )
}
