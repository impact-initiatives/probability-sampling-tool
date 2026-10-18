page_navbar(
  title = "Probability sampling tool",
  id = "main_nav",
  lang = "en",
  theme = bs_theme(
    version = 5,
    # WCAG AA: white text on primary/danger/success is above 4.5:1
    primary = "#1A56A0",
    secondary = "#5C6670",
    success = "#146C43",
    danger = "#B42318",
    "body-bg" = "#F5F7FA",
    # form control borders need 3:1 against the background (WCAG 1.4.11)
    "input-border-color" = "#8A939C",
    "card-cap-bg" = "#EEF3FA",
    "card-cap-color" = "#12355B",
    base_font = font_collection(
      "system-ui",
      "-apple-system",
      "Segoe UI",
      "Roboto",
      "Helvetica Neue",
      "Arial",
      "sans-serif"
    ),
    # the Bootstrap 3 base size, to keep the setup card on one screen
    "font-size-base" = "0.875rem",
    # the Bootstrap 3 paragraph spacing, which keeps the Introduction compact
    "paragraph-margin-bottom" = "0.7rem",
    "h1-font-size" = "2rem",
    "h2-font-size" = "1.4rem"
  ),
  navbar_options = navbar_options(bg = "#12355B", theme = "dark"),
  fillable = FALSE,
  header = tagList(
    shinyjs::useShinyjs(),
    # Spinner on the outputs being recalculated and a pulse while the app is busy
    useBusyIndicators(),
    # Shiny drops a file above the size limit silently: report its name to the
    # server (input$popdata_too_large) and do not upload it
    tags$script(HTML(sprintf(
      "document.addEventListener('change', function(e) {
				if (e.target.id !== 'popdata') return;
				var big = Array.from(e.target.files).find(function(f) { return f.size > %d; });
				Shiny.setInputValue('popdata_too_large', big ? big.name : null, {priority: 'event'});
				if (big) { e.stopImmediatePropagation(); }
			}, true);",
      as.integer(MAX_UPLOAD_MB * 1024^2)
    ))),
    # Cache-bust: without a version query string, browsers keep serving a
    # previously-cached app.css after it changes on disk.
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0("app.css?v=", as.integer(file.mtime("www/app.css")))
    )
  ),
  nav_panel(
    "Guide",
    withMathJax(
      div(
        class = "guide",
        navset_pill(
          id = "guide_nav",
          nav_panel("Start", value = "start", guide_start()),
          nav_panel(
            "Sampling types",
            value = "types",
            accordion(
              id = "types_accordion",
              open = FALSE,
              multiple = FALSE,
              accordion_panel(
                tagList(
                  "Simple random sampling",
                  span(
                    class = "badge text-bg-light fw-normal ms-2",
                    "before: Simple random"
                  )
                ),
                value = "simple_random",
                guide_text("guide/type-simple-random.md")
              ),
              accordion_panel(
                tagList(
                  "PPS sampling with replacement",
                  span(
                    class = "badge text-bg-light fw-normal ms-2",
                    "before: Simple random - allocation"
                  )
                ),
                value = "pps",
                guide_text("guide/type-pps.md")
              ),
              accordion_panel(
                tagList(
                  "Two-stage PPS cluster sampling with replacement",
                  span(
                    class = "badge text-bg-light fw-normal ms-2",
                    "before: Cluster sampling"
                  )
                ),
                value = "cluster",
                guide_text("guide/type-cluster.md")
              )
            )
          ),
          nav_panel(
            "Your data",
            value = "data",
            layout_columns(
              col_widths = c(5, 7),
              fill = FALSE,
              guide_card("The file", "guide/data-file.md"),
              guide_card(
                "What each type expects",
                "guide/data-types.md"
              )
            ),
            guide_checks()
          ),
          nav_panel(
            "Parameters",
            value = "parameters",
            guide_card("Mode of sampling", "guide/parameters-mode.md"),
            guide_card("Parameters", "guide/parameters-list.md"),
            guide_card(
              "Which parameters apply",
              "guide/parameters-applies.md"
            )
          ),
          nav_panel(
            "Results",
            value = "results",
            guide_card("The tables", "guide/results-tables.md"),
            guide_card(
              "Columns added by the tool",
              "guide/results-columns.md"
            ),
            guide_card("Sample summary", "guide/results-summary.md")
          ),
          nav_panel(
            "What's new",
            value = "news",
            guide_sections("NEWS.md", "27rem")
          ),
          nav_panel(
            "Methods",
            value = "methods",
            guide_sections("guide/methods.md", "30rem")
          )
        )
      )
    )
  ),
  nav_panel(
    "Sampling frame",
    layout_sidebar(
      fill = FALSE,
      fillable = FALSE,
      padding = "1rem",
      sidebar = sidebar(
        id = "setup_sidebar",
        width = 600,
        open = "desktop",
        resizable = TRUE,
        fillable = FALSE,
        gap = 0,
        padding = 0,
        sidebar_header("Sampling setup"),
        div(
          class = "p-3",
          fluidRow(
            column(
              12,
              select_input_overlay(
                "topup",
                "Mode of sampling",
                c(
                  "Enter sample size",
                  "Sample size based on population"
                ),
                "Sample size based on population"
              )
            ),
            column(
              12,
              select_input_overlay(
                "samp_type",
                "Type of sampling",
                c(
                  "Simple random sampling",
                  "PPS sampling with replacement",
                  "Two-stage PPS cluster sampling with replacement"
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
                c("Not stratified", "Stratified")
              )
            ),
            conditionalPanel(
              # the buffer only applies to a sample size computed from the population
              condition = "input.topup != 'Enter sample size'",
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
          ),
          # min/max mirror validate_parameters(); HTML cannot express a strict bound,
          # so the closest value on the step grid is used.
          fluidRow(
            conditionalPanel(
              condition = "input.topup != 'Enter sample size'",
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
              condition = "input.topup != 'Enter sample size'",
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
              condition = "input.topup != 'Enter sample size'",
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
              condition = "input.topup == 'Enter sample size'",
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
          ),
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
                "Upload CSV" = "upload",
                "Use test data" = "test"
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
                condition = "input.data_source == 'upload'",
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
                condition = "input.data_source == 'test'",
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
          uiOutput("file_error"),
          # Simple random sampling without stratification needs no column from the data
          conditionalPanel(
            condition = "input.samp_type != 'Simple random sampling' || input.stratified == 'Stratified'",
            div(class = "param-subhead", "Column mapping"),
            fluidRow(
              conditionalPanel(
                condition = "input.samp_type == 'Two-stage PPS cluster sampling with replacement'",
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
                condition = "input.samp_type == 'Two-stage PPS cluster sampling with replacement'|input.samp_type =='PPS sampling with replacement'",
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
                condition = "input.stratified == 'Stratified'",
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
          ),
          conditionalPanel(
            condition = "input.samp_type == 'Two-stage PPS cluster sampling with replacement'",
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
          ),
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
      ),
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        fill = FALSE,
        card(
          card_header(h2("Target sampling", class = "h6 mb-0")),
          card_body(
            fillable = FALSE,
            uiOutput("apply_error"),
            DT::dataTableOutput("target_frame")
          )
        ),
        card(
          card_header(h2("Sampling frame", class = "h6 mb-0")),
          card_body(
            fillable = FALSE,
            DT::dataTableOutput("sampling_frame")
          )
        )
      )
    )
  ),
  nav_panel(
    "Sampling",
    layout_sidebar(
      fill = FALSE,
      fillable = FALSE,
      padding = "1rem",
      sidebar = sidebar(
        id = "sampling_sidebar",
        width = 300,
        open = "desktop",
        resizable = TRUE,
        fillable = FALSE,
        gap = 0,
        padding = 0,
        sidebar_header("Actions"),
        div(
          class = "p-3",
          shinyjs::disabled(
            actionButton(
              "desButton",
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
        card(
          card_header(h2("Sample summary", class = "h6 mb-0")),
          card_body(
            fillable = FALSE,
            uiOutput("sample_error"),
            DT::dataTableOutput("test_1")
          )
        ),
        card(
          card_header(h2("Sample", class = "h6 mb-0")),
          card_body(
            fillable = FALSE,
            DT::dataTableOutput("sampling_output")
          )
        )
      )
    )
  )
)
