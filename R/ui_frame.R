# The "Sampling frame" tab: the setup of the sampling and the two tables it fills.

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
          DT::dataTableOutput("target_frame")
        ),
        table_card("Sampling frame", DT::dataTableOutput("sampling_frame"))
      )
    )
  )
}

# The setup form, in the order of the steps of the user
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
          "f_apply",
          "Apply",
          label_busy = "Applying...",
          style = "width: 200px;"
        )
      )
    )
  )
}

# Mode and type of sampling, stratification, buffer and seed
setup_sampling_choice <- function() {
  tagList(
    fluidRow(
      column(
        12,
        select_input_overlay(
          "topup",
          "Mode of sampling",
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
          "samp_type",
          "Type of sampling",
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
        select_input_overlay(
          "stratified",
          "Stratified ?",
          c(NOT_STRATIFIED, STRATIFIED)
        )
      ),
      conditionalPanel(
        # the buffer only applies to a sample size computed from the population
        condition = js_differs("topup", SAMPLING_MODE_ENTER_SIZE),
        column(
          4,
          numeric_input_with_tip(
            'buf',
            "Buffer",
            tip = "Extra share added to the target to cover non-response or unusable interviews: target \u00d7 (1 + buffer). Base it on the non-response rate you expect.",
            value = 0.05,
            min = 0,
            step = 0.01
          )
        )
      ),
      column(
        4,
        numeric_input_with_tip(
          'seed_value',
          "Seed",
          tip = "Starting point of the random draw. The same seed with the same settings gives the same sample, so a draw can be reproduced. Change it to get a different sample.",
          value = 1,
          min = -2147483647,
          max = 2147483647,
          step = 1
        )
      )
    )
  )
}

# Parameters of the sample size, or the target typed by the user
setup_sample_size <- function() {
  # min/max mirror validate_parameters(); HTML cannot express a strict bound,
  # so the closest value on the step grid is used.
  fluidRow(
    conditionalPanel(
      condition = js_differs("topup", SAMPLING_MODE_ENTER_SIZE),
      column(
        4,
        numeric_input_with_tip(
          'conf_level',
          "Confidence level",
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
      condition = js_differs("topup", SAMPLING_MODE_ENTER_SIZE),
      column(
        4,
        numeric_input_with_tip(
          'e_marg',
          "Error Margin",
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
      condition = js_differs("topup", SAMPLING_MODE_ENTER_SIZE),
      column(
        4,
        # fixed in the computation (FIXED_PROPORTION): shown, not editable
        htmltools::tagQuery(
          numeric_input_with_tip(
            'pror',
            "Proportion",
            tip = "Fixed at 0.5, the most cautious value: it gives the largest sample size. See the Guide for details.",
            value = FIXED_PROPORTION
          )
        )$find("input")$addAttrs(disabled = NA)$allTags()
      )
    ),
    conditionalPanel(
      condition = js_equals("topup", SAMPLING_MODE_ENTER_SIZE),
      column(
        4,
        numericInput(
          'target',
          'Enter target sample size',
          NULL,
          min = 1,
          step = 1
        )
      )
    )
  )
}

# Where the sampling frame comes from, and the feedback on the file
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
          # aria-label on the inputs: a hidden label would still take up space
          htmltools::tagQuery(
            fileInput(
              'popdata',
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

# The columns of the frame the type of sampling needs
setup_column_mapping <- function() {
  # Simple random sampling without stratification needs no column from the data
  conditionalPanel(
    condition = paste(
      js_differs("samp_type", SAMPLING_TYPE_SIMPLE),
      "||",
      js_equals("stratified", STRATIFIED)
    ),
    div(class = "param-subhead", "Column mapping"),
    fluidRow(
      conditionalPanel(
        condition = js_equals("samp_type", SAMPLING_TYPE_CLUSTER),
        column(
          4,
          select_input_overlay(
            'col_psu',
            'Input cluster',
            choices = NULL
          )
        )
      ),
      conditionalPanel(
        condition = paste(
          js_equals("samp_type", SAMPLING_TYPE_CLUSTER),
          "||",
          js_equals("samp_type", SAMPLING_TYPE_PPS)
        ),
        column(
          4,
          select_input_overlay(
            'colpop',
            'Input population',
            choices = NULL
          )
        )
      ),
      # last, so that the two other fields keep their place
      # whether the sample is stratified or not
      conditionalPanel(
        condition = js_equals("stratified", STRATIFIED),
        column(
          4,
          select_input_overlay(
            'strata',
            'Input strata',
            choices = NULL,
            multiple = F
          )
        )
      )
    )
  )
}

# Cluster size and ICC
setup_cluster_design <- function() {
  conditionalPanel(
    condition = js_equals("samp_type", SAMPLING_TYPE_CLUSTER),
    div(class = "param-subhead", "Cluster design"),
    fluidRow(
      column(
        6,
        numeric_input_with_tip(
          'cls',
          "Cluster size",
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
          'ICC',
          "ICC",
          tip = "Intracluster correlation: how alike the units of a cluster are (0 = not at all, 1 = identical). It inflates the sample through the design effect, 1 + (cluster size \u2212 1) \u00d7 ICC. Use a value from a similar past survey.",
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
