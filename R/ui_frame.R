# The "Sampling frame" tab: the setup of the sampling and the two tables it fills.

#' Build the Sampling frame tab
#'
#' The setup of the sampling in a sidebar, and the two tables it fills: the
#' targets and the sampling frame.
#'
#' @returns A `nav_panel`.
#' @noRd
frame_tab <- function() {
  nav_panel(
    "Sampling frame",
    layout_sidebar(
      fill = FALSE,
      fillable = FALSE,
      padding = "1rem",
      sidebar = setup_sidebar(),
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        fill = FALSE,
        table_card(
          "Target sampling",
          uiOutput("apply_error"),
          DT::dataTableOutput("target_table")
        ),
        table_card("Sampling frame", DT::dataTableOutput("frame_table"))
      )
    )
  )
}

#' Build the setup form
#'
#' Its sections follow the steps of the user.
#'
#' @returns A sidebar.
#' @noRd
setup_sidebar <- function() {
  tab_sidebar(
    id = "setup_sidebar",
    width = 600,
    sidebar_header("Sampling setup"),
    div(
      class = "p-3",
      setup_sampling_choice(),
      setup_sample_size(),
      setup_data_source(),
      setup_column_mapping(),
      setup_cluster_design(),
      div(
        class = "text-center mt-2",
        input_task_button(
          "apply_button",
          "Apply",
          label_busy = "Applying...",
          style = "width: 200px;"
        )
      )
    )
  )
}

#' Build the choices of the sampling
#'
#' Mode and type of sampling, stratification, buffer and seed.
#'
#' @returns The fields, as HTML.
#' @noRd
setup_sampling_choice <- function() {
  tagList(
    fluidRow(
      column(
        12,
        select_input_overlay(
          "sampling_mode",
          FIELD_LABELS[["sampling_mode"]],
          c(
            SAMPLING_MODE_ENTER_SIZE,
            SAMPLING_MODE_FROM_POPULATION
          ),
          SAMPLING_MODE_FROM_POPULATION
        )
      ),
      column(
        12,
        select_input_overlay(
          "sampling_type",
          FIELD_LABELS[["sampling_type"]],
          c(
            SAMPLING_TYPE_SIMPLE,
            SAMPLING_TYPE_PPS,
            SAMPLING_TYPE_CLUSTER
          )
        )
      )
    ),
    fluidRow(
      column(
        4,
        # the label above the box, like the other fields
        div(
          class = "stratified-check",
          tags$label(
            class = "control-label",
            `for` = "stratified",
            FIELD_LABELS[["stratified"]]
          ),
          checkboxInput("stratified", NULL, value = FALSE)
        )
      ),
      conditionalPanel(
        # the buffer only applies to a sample size computed from the population
        condition = js_differs("sampling_mode", SAMPLING_MODE_ENTER_SIZE),
        column(
          4,
          numeric_input_with_tip(
            'buffer',
            FIELD_LABELS[["buffer"]],
            tip = paste0(
              "A share of the target added to cover non-response or unusable interviews, not a number of surveys: 0.05 adds 5%, 0.5 adds 50% (maximum ",
              MAX_BUFFER,
              ", i.e. ",
              MAX_BUFFER * 100,
              "%). Requested target = target \u00d7 (1 + buffer)."
            ),
            value = 0.05,
            min = 0,
            max = MAX_BUFFER,
            step = 0.01
          )
        )
      ),
      column(
        4,
        numeric_input_with_tip(
          'seed',
          FIELD_LABELS[["seed"]],
          tip = "Starting point of the random draw. The same seed with the same settings gives the same sample, so a draw can be reproduced.",
          value = 1,
          min = -2147483647,
          max = 2147483647,
          step = 1
        )
      )
    )
  )
}

#' Build the parameters of the sample size
#'
#' Confidence level, error margin and proportion, or the target typed by the user.
#' The minimum and maximum of the fields mirror `validate_parameters()`; HTML
#' cannot express a strict bound, so the closest value on the step grid is used.
#'
#' @returns The fields, as HTML.
#' @noRd
setup_sample_size <- function() {
  # min/max mirror validate_parameters(); HTML cannot express a strict bound,
  # so the closest value on the step grid is used.
  fluidRow(
    conditionalPanel(
      condition = js_differs("sampling_mode", SAMPLING_MODE_ENTER_SIZE),
      column(
        4,
        numeric_input_with_tip(
          'confidence_level',
          FIELD_LABELS[["confidence_level"]],
          tip = "How often the interval around your estimate would contain the true value if the survey were repeated (0.95 = 95% of the time). Higher confidence needs a larger sample.",
          source = COCHRAN_1977,
          value = 0.95,
          min = 0.005,
          max = 0.995,
          step = 0.005
        )
      )
    ),
    conditionalPanel(
      condition = js_differs("sampling_mode", SAMPLING_MODE_ENTER_SIZE),
      column(
        4,
        numeric_input_with_tip(
          'error_margin',
          FIELD_LABELS[["error_margin"]],
          tip = "Largest gap you accept between the estimate and the true value, at the chosen confidence level (0.05 = \u00b15 percentage points). A smaller margin needs a larger sample.",
          source = COCHRAN_1977,
          value = 0.05,
          min = 0.005,
          max = 0.995,
          step = 0.005
        )
      )
    ),
    conditionalPanel(
      condition = js_differs("sampling_mode", SAMPLING_MODE_ENTER_SIZE),
      column(
        4,
        # fixed in the computation (FIXED_PROPORTION): shown, not editable
        htmltools::tagQuery(
          numeric_input_with_tip(
            'proportion',
            FIELD_LABELS[["proportion"]],
            tip = "Fixed at 0.5, the most cautious value: it gives the largest sample size. See the Guide for details.",
            value = FIXED_PROPORTION
          )
        )$find("input")$addAttrs(disabled = NA)$allTags()
      )
    ),
    conditionalPanel(
      condition = js_equals("sampling_mode", SAMPLING_MODE_ENTER_SIZE),
      column(
        4,
        numericInput(
          'target_size',
          FIELD_LABELS[["target_size"]],
          NULL,
          min = 1,
          step = 1
        )
      )
    )
  )
}

#' Build the choice of the data source
#'
#' Where the sampling frame comes from, and the feedback on the file.
#'
#' @returns The fields, as HTML.
#' @noRd
setup_data_source <- function() {
  tagList(
    htmltools::tagQuery(
      radioButtons(
        "data_source",
        tagList(
          tags$span(
            id = "data_source-name",
            class = "param-subhead",
            "Data source"
          ),
          info_tip(
            "Data source",
            "One CSV file holds a single table. If your data is in an Excel workbook, save the sheet you need as a CSV file first: only .csv files are accepted."
          )
        ),
        choices = c(
          "Upload CSV" = DATA_SOURCE_UPLOAD,
          "Use test data" = DATA_SOURCE_TEST
        ),
        inline = TRUE
      )
    )$removeAttrs("aria-labelledby")$addAttrs(
      "aria-labelledby" = "data_source-name"
    )$allTags(),
    fluidRow(
      class = "mt-2",
      column(
        7,
        conditionalPanel(
          condition = js_equals("data_source", DATA_SOURCE_UPLOAD),
          # aria-label on the inputs: a hidden label would still take up space.
          # autocomplete off on the box that shows the name of the file: a browser
          # could put the name back after a reload, when the file is gone.
          htmltools::tagQuery(
            fileInput(
              'frame_file',
              NULL,
              accept = c(
                'text/csv',
                'text/comma-separated-values,text/plain'
              ),
              placeholder = paste0(
                "CSV, max ",
                MAX_UPLOAD_MB,
                " MB"
              )
            )
          )$find("input")$addAttrs(
            "aria-label" = "CSV file"
          )$resetSelected()$find("input.form-control")$addAttrs(
            autocomplete = "off"
          )$allTags()
        ),
        conditionalPanel(
          condition = js_equals("data_source", DATA_SOURCE_TEST),
          div(
            class = "example-link",
            downloadLink(
              "download_example",
              tagList(
                icon("download"),
                "Download example CSV"
              )
            )
          )
        )
      ),
      column(5, uiOutput("data_status"))
    ),
    uiOutput("file_error")
  )
}

#' Build the choice of the columns of the frame
#'
#' Only the columns that the type of sampling needs are shown: simple random
#' sampling without stratification needs none.
#'
#' @returns The fields, as HTML.
#' @noRd
setup_column_mapping <- function() {
  # Simple random sampling without stratification needs no column from the data
  conditionalPanel(
    condition = paste(
      js_differs("sampling_type", SAMPLING_TYPE_SIMPLE),
      "||",
      "input.stratified"
    ),
    div(class = "param-subhead", "Column mapping"),
    fluidRow(
      conditionalPanel(
        condition = js_equals("sampling_type", SAMPLING_TYPE_CLUSTER),
        column(
          4,
          select_input_overlay(
            'psu_column',
            FIELD_LABELS[["psu_column"]],
            choices = NULL
          )
        )
      ),
      conditionalPanel(
        condition = paste(
          js_equals("sampling_type", SAMPLING_TYPE_CLUSTER),
          "||",
          js_equals("sampling_type", SAMPLING_TYPE_PPS)
        ),
        column(
          4,
          select_input_overlay(
            'population_column',
            FIELD_LABELS[["population_column"]],
            choices = NULL
          )
        )
      ),
      # last, so that the two other fields keep their place
      # whether the sample is stratified or not
      conditionalPanel(
        condition = "input.stratified",
        column(
          4,
          select_input_overlay(
            'strata_column',
            FIELD_LABELS[["strata_column"]],
            choices = NULL,
            multiple = FALSE
          )
        )
      )
    )
  )
}

#' Build the cluster design fields
#'
#' Cluster size and ICC, for the cluster type.
#'
#' @returns The fields, as HTML.
#' @noRd
setup_cluster_design <- function() {
  conditionalPanel(
    condition = js_equals("sampling_type", SAMPLING_TYPE_CLUSTER),
    div(class = "param-subhead", "Cluster design"),
    fluidRow(
      column(
        6,
        numeric_input_with_tip(
          'cluster_size',
          FIELD_LABELS[["cluster_size"]],
          tip = "Number of interviews planned in each selected cluster (at least 2). Larger clusters lower the fieldwork cost but raise the design effect.",
          source = KISH_1965,
          value = 5,
          min = 2,
          step = 1
        )
      ),
      column(
        6,
        numeric_input_with_tip(
          'icc',
          FIELD_LABELS[["icc"]],
          tip = "Intracluster correlation: how alike the units of a cluster are (0 = not at all, 1 = identical). It inflates the computed sample size through the design effect, 1 + (cluster size \u2212 1) \u00d7 ICC. With \"Enter sample size\" the size is not changed: the design effect and the effective sample size are shown in the summary. Use a value from a similar past survey.",
          source = KISH_1965,
          value = 0.06,
          min = 0,
          max = 1,
          step = 0.01
        )
      )
    )
  )
}
