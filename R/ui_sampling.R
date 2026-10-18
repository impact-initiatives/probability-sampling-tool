# The "Sampling" tab: the draw, the exports and the two tables of the result.

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
          )
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
          DT::dataTableOutput("test_1")
        ),
        table_card("Sample", DT::dataTableOutput("sampling_output"))
      )
    )
  )
}
