library(markdown)
library(bslib)

# Kish, L. (1965). Survey Sampling. New York: Wiley.
kish_1965 <- tagList("Kish, ", tags$em("Survey Sampling"), " (1965)")
# Cochran, W. G. (1977). Sampling Techniques, 3rd ed. New York: Wiley.
cochran_1977 <- tagList("Cochran, ", tags$em("Sampling Techniques"), " (1977)")

# Header of a sidebar section, identical to the card headers of the main area
sidebar_header <- function(text) {
  div(
    class = "card bslib-card border-0 rounded-0 bg-transparent",
    card_header(h2(text, class = "h6 mb-0"))
  )
}

# Help icon opening a tooltip, to put inside a field label.
info_tip <- function(label, tip, source = NULL) {
  tooltip(
    tags$span(
      class = "info-tip",
      tabindex = "0",
      role = "img",
      `aria-label` = paste("About", label),
      icon("circle-info", `aria-hidden` = "true")
    ),
    tip,
    if (!is.null(source)) div(class = "info-tip-source", source)
  )
}

# numericInput() whose label carries a help icon with a tooltip. The input is
# named by the label text only, not by the icon.
numeric_input_with_tip <- function(inputId, label, tip, source = NULL, ...) {
  name_id <- paste0(inputId, "-name")
  htmltools::tagQuery(
    numericInput(
      inputId,
      tagList(
        tags$span(id = name_id, label),
        info_tip(label, tip, source)
      ),
      ...
    )
  )$find("input")$addAttrs("aria-labelledby" = name_id)$allTags()
}

# selectInput() whose list opens over the page. Inside the setup sidebar, which
# scrolls its content, a list opening near the bottom of the card is cut off.
select_input_overlay <- function(inputId, label, choices = NULL, ...) {
  selectizeInput(
    inputId,
    label,
    choices = choices,
    ...,
    options = list(dropdownParent = "body")
  )
}

# Text of the guide: Markdown files in guide/ (and NEWS.md), so that it can be
# reviewed and corrected without touching the code. {{max_upload_mb}} is filled in.
guide_markdown <- function(text) {
  text <- gsub("{{max_upload_mb}}", max_upload_mb, text, fixed = TRUE)
  # markdown::mark() turns $...$ into \(...\) for MathJax, unlike shiny::markdown()
  div(
    class = "guide-text",
    HTML(markdown::mark(text = text, format = "html", template = FALSE))
  )
}

guide_text <- function(file) {
  guide_markdown(paste(readLines(file, warn = FALSE), collapse = "\n"))
}

# A Markdown file shown as one card per second level heading, in a grid. What
# comes before the first heading is shown above the cards.
guide_sections <- function(file, width = "30rem") {
  lines <- readLines(file, warn = FALSE)
  starts <- grep("^## ", lines)
  intro <- if (starts[1] > 1) lines[seq_len(starts[1] - 1)] else character()
  ends <- c(starts[-1] - 1, length(lines))
  cards <- lapply(seq_along(starts), function(i) {
    body <- lines[(starts[i] + 1):ends[i]]
    card(
      class = "guide-card h-100",
      fill = FALSE,
      card_header(h3(
        sub("^## ", "", lines[starts[i]]),
        class = "h6 fw-semibold mb-0"
      )),
      card_body(fillable = FALSE, guide_markdown(paste(body, collapse = "\n")))
    )
  })
  tagList(
    if (length(intro) > 0 && any(nzchar(intro))) {
      guide_markdown(paste(intro, collapse = "\n"))
    },
    do.call(
      layout_column_wrap,
      c(list(width = width, fill = FALSE, heights_equal = "row"), cards)
    )
  )
}

# A titled block of the guide, around a Markdown file
guide_card <- function(title, file) {
  card(
    class = "guide-card h-100",
    fill = FALSE,
    card_header(h3(title, class = "h6 fw-semibold mb-0")),
    card_body(fillable = FALSE, guide_text(file))
  )
}

# The checks of the app, by group, closed until opened
guide_checks <- function() {
  card(
    class = "guide-card",
    fill = FALSE,
    card_header(h3("What the tool checks", class = "h6 fw-semibold mb-0")),
    card_body(
      fillable = FALSE,
      accordion(
        id = "checks_accordion",
        open = FALSE,
        multiple = TRUE,
        accordion_panel(
          "The file",
          value = "file",
          guide_text("guide/checks-file.md")
        ),
        accordion_panel(
          "The columns",
          value = "columns",
          guide_text("guide/checks-columns.md")
        ),
        accordion_panel(
          "The parameters",
          value = "parameters",
          guide_text("guide/checks-parameters.md")
        ),
        accordion_panel(
          "The cluster design",
          value = "cluster",
          guide_text("guide/checks-cluster.md")
        ),
        accordion_panel(
          "The draw",
          value = "draw",
          guide_text("guide/checks-draw.md")
        )
      )
    )
  )
}

# A step of the "How it works" row of the guide; the link sits at the bottom
guide_step <- function(number, title, text, link = NULL) {
  card(
    class = "guide-step",
    card_body(
      class = "d-flex flex-column",
      div(
        class = "d-flex align-items-center gap-2 mb-1",
        span(class = "guide-step-number", number),
        h3(title, class = "h6 fw-semibold mb-0")
      ),
      p(text, class = "small mb-2"),
      if (!is.null(link)) div(class = "mt-auto small", link)
    )
  )
}

# A card of the "Three sampling types" row of the guide; the link sits at the bottom
guide_type_card <- function(name, before, one_row, needs, button_id) {
  card(
    class = "guide-type",
    card_header(
      div(
        h3(name, class = "h6 fw-semibold mb-1"),
        span(class = "badge text-bg-light fw-normal", paste("before:", before))
      )
    ),
    card_body(
      class = "d-flex flex-column",
      p(strong("One row is "), one_row, class = "small mb-1"),
      p(strong("Needs "), needs, class = "small mb-2"),
      div(
        class = "mt-auto small",
        actionLink(button_id, tagList("Details ", icon("arrow-right")))
      )
    )
  )
}

# The landing of the guide
guide_start <- function() {
  tagList(
    div(
      class = "guide-hero",
      h1("Draw a probability sample from your sampling frame"),
      p(
        class = "lead",
        "Load your CSV, choose a sampling type and set the parameters. The tool returns the units to survey and the number of surveys to run in each."
      ),
      actionButton(
        "guide_to_app",
        tagList("Start sampling ", icon("arrow-right")),
        class = "btn-primary"
      )
    ),
    h2("How it works", class = "h6 fw-semibold mt-3"),
    layout_column_wrap(
      width = "14rem",
      fill = FALSE,
      guide_step(
        1,
        "Prepare the frame",
        "One CSV, one row per unit of your frame.",
        actionLink("guide_to_data", "What the frame must contain")
      ),
      guide_step(
        2,
        "Set up",
        "Mode, type of sampling, columns and parameters in the Sampling frame tab.",
        actionLink("guide_to_parameters", "The parameters")
      ),
      guide_step(
        3,
        "Apply",
        "Computes the target per stratum and checks the frame and the parameters."
      ),
      guide_step(
        4,
        "Sample and export",
        "Draws the sample in the Sampling tab, then download the Sample and its summary.",
        actionLink("guide_to_results", "Reading the results")
      )
    ),
    h2("Three sampling types", class = "h6 fw-semibold mt-3"),
    layout_column_wrap(
      width = "17rem",
      fill = FALSE,
      guide_type_card(
        "Simple random sampling",
        "Simple random",
        "a unit to survey (household, person).",
        "no population column.",
        "guide_open_simple_random"
      ),
      guide_type_card(
        "PPS sampling with replacement",
        "Simple random - allocation",
        "a PSU (an area) with its population.",
        "a population column.",
        "guide_open_pps"
      ),
      guide_type_card(
        "Two-stage PPS cluster sampling with replacement",
        "Cluster sampling",
        "a PSU with its population.",
        "a cluster column and a population column.",
        "guide_open_cluster"
      )
    ),
    div(
      class = "alert alert-info mt-3 mb-0 py-2 d-flex justify-content-between align-items-center",
      span(
        icon("circle-info"),
        strong(" Coming from the previous version? "),
        "The sampling types and the columns of the results have new names."
      ),
      actionLink("guide_to_news", "What's new")
    )
  )
}

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
      as.integer(max_upload_mb * 1024^2)
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
                  source = cochran_1977,
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
                  source = cochran_1977,
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
                # fixed in the computation (fixed_proportion): shown, not editable
                htmltools::tagQuery(
                  numeric_input_with_tip(
                    'pror',
                    "Proportion",
                    tip = "Fixed at 0.5, the most cautious value: it gives the largest sample size. See the Guide for details.",
                    value = fixed_proportion
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
                      max_upload_mb,
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
                  source = kish_1965,
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
                  source = kish_1965,
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
