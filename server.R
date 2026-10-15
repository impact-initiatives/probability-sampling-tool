# This function is the server logic for the Probability Sampling Tool application.
# It defines the reactive behavior and output rendering for the user interface.

function(input, output, session) {
  # give each session a random default seed, visible and editable by the user
  updateNumericInput(session, "seed_value", value = sample.int(1e6, 1))

  # The links of the guide: to its own sections, to the types, and to the app
  guide_sections <- c(
    guide_to_data = "data",
    guide_to_parameters = "parameters",
    guide_to_results = "results",
    guide_to_news = "news"
  )
  guide_types <- c(
    guide_open_simple_random = "simple_random",
    guide_open_pps = "pps",
    guide_open_cluster = "cluster"
  )
  lapply(names(guide_sections), function(id) {
    observeEvent(input[[id]], bslib::nav_select("guide_nav", guide_sections[[id]]))
  })
  lapply(names(guide_types), function(id) {
    observeEvent(input[[id]], {
      bslib::nav_select("guide_nav", "types")
      bslib::accordion_panel_set("types_accordion", guide_types[[id]])
    })
  })
  observeEvent(input$guide_to_app, bslib::nav_select("main_nav", "Sampling frame"))

  # an uploaded file is either a usable CSV or an error message, never a crash
  upload <- reactive({
    if (!is.null(input$popdata_too_large)) {
      return(list(
        data = NULL,
        error = csv_format_error(input$popdata_too_large) %||%
          paste0(
            "'",
            input$popdata_too_large,
            "' is larger than the ",
            max_upload_mb,
            " MB limit."
          )
      ))
    }
    f <- input$popdata
    if (is.null(f)) {
      return(NULL)
    }
    read_uploaded_csv(f$name, f$datapath)
  })

  file_error <- reactive(if (input$data_source == "upload") upload()$error)

  # get the data
  db <- reactive({
    if (input$data_source == "test") {
      fread("data_example.csv")
    } else {
      upload()$data
    }
  })

  output$file_error <- renderUI({
    if (!is.null(file_error())) {
      div(class = "alert alert-danger py-2 mb-0 mt-2", file_error())
    }
  })

  # one-line feedback under the data source selector
  output$data_status <- renderUI({
    d <- db()
    req(d)
    src <- if (input$data_source == "test") {
      "data_example.csv"
    } else {
      input$popdata$name
    }
    div(
      class = "data-status",
      paste0(
        "\u2714 ",
        src,
        " - ",
        format(nrow(d), big.mark = ","),
        " rows x ",
        ncol(d),
        " columns"
      )
    )
  })

  output$download_example <- downloadHandler(
    filename = "data_example.csv",
    content = function(file) file.copy("data_example.csv", file)
  )

  # update the check ox based on the data headings
  observe({
    updateSelectInput(
      session,
      "col_psu",
      choices = c("None", colnames(db())),
      selected = "None"
    )
    updateSelectInput(
      session,
      "strata",
      choices = c("None", colnames(db())),
      selected = "None"
    )
    updateSelectInput(
      session,
      "colpop",
      choices = c("None", colnames(db())),
      selected = "None"
    )
  })

  # Stops with the message when a validate_*() check returned one
  require_valid <- function(msg) validate(need(is.null(msg), msg))

  # NULL when `expr` succeeds, otherwise its validation message ("" if silent).
  validation_message <- function(expr) {
    tryCatch(
      {
        expr
        NULL
      },
      shiny.silent.error = function(e) conditionMessage(e)
    )
  }

  # Observers must not evaluate an `expr` that can fail without catching it:
  # an unexpected error there would end the session.
  succeeds <- function(expr) {
    tryCatch(
      {
        expr
        TRUE
      },
      error = function(e) FALSE
    )
  }

  # a new upload hides the frame and target tables until the next Apply
  tables_stale <- reactiveVal(FALSE)
  observeEvent(
    list(input$popdata, input$popdata_too_large),
    tables_stale(TRUE),
    ignoreInit = TRUE
  )
  # what Apply found wrong in the data, for the downloadable list
  issue_list <- reactiveVal(NULL)
  # same for a draw blocked because a PSU is assigned too many surveys
  draw_issues <- reactiveVal(NULL)
  observeEvent(
    input$f_apply,
    {
      tables_stale(FALSE)
      issue_list(NULL)
      draw_issues(NULL)
    },
    priority = 10
  )

  # create the sampling frame
  frame <- eventReactive(input$f_apply, {
    require_valid(validate_strata_selection(input))
    require_valid(file_error())
    sframe <- db()
    validate(need(
      !is.null(sframe),
      "Please upload a CSV file before applying."
    ))
    require_valid(validate_reserved_columns(sframe))
    require_valid(validate_psu_column(sframe, input))
    require_valid(validate_population_column(sframe, input))
    pop_issues <- population_issues(sframe, input)
    issue_list(pop_issues)
    require_valid(population_message(pop_issues, input$colpop))
    require_valid(validate_parameters(input))
    # not needed for the frame, but reported here rather than at the draw
    require_valid(validate_seed(input$seed_value))
    format_sampling_frame(sframe, input)
  })

  # create the target sampling size by strata
  cible <- eventReactive(input$f_apply, {
    targets <- create_targets(frame(), input)
    issues <- cluster_size_issues(frame(), targets, input)
    issue_list(issues)
    require_valid(cluster_size_message(issues, input$cls))
    targets
  })

  # "Sample" is usable after a successful Apply, until a parameter changes or a
  # draw fails; an impossible seed blocks it too.
  applied <- reactiveVal(FALSE)
  draw_failed <- reactiveVal(FALSE)

  # The sample on screen is the one drawn after the last Apply: a new Apply
  # hides it and its downloads until Sample is clicked again. Changing a
  # parameter without Apply leaves it, like the frame and target tables.
  sample_current <- reactiveVal(FALSE)
  observeEvent(input$f_apply, sample_current(FALSE), priority = 10)

  observeEvent(input$f_apply, {
    applied(FALSE)
    draw_failed(FALSE)
    # the errors are shown by the outputs; here they must not end the session
    if (!succeeds(frame()) || !succeeds(cible())) {
      return()
    }
    applied(TRUE)

    # cluster sampling never caps the draw: cible() already blocks a target
    # above the room of a stratum
    affected <- if (input$samp_type != "Two-stage PPS cluster sampling with replacement") {
      check_target_vs_population(cible())
    }
    if (!is.null(affected)) showModal(exceeds_population_modal(affected))
  })

  # any change to a frame- or target-defining input invalidates the previous Apply
  observeEvent(
    list(
      input$popdata,
      input$popdata_too_large,
      input$data_source,
      input$samp_type,
      input$stratified,
      input$col_psu,
      input$strata,
      input$colpop,
      input$topup,
      input$target,
      input$conf_level,
      input$e_marg,
      input$buf,
      input$cls,
      input$ICC
    ),
    applied(FALSE),
    ignoreInit = TRUE
  )

  observeEvent(input$desButton, {
    drawn <- succeeds(out())
    sample_current(drawn)
    if (!drawn) {
      draw_failed(TRUE)
      return()
    }
    # same warning as at Apply: the draw has just been capped
    affected <- if (
      input$samp_type != "Two-stage PPS cluster sampling with replacement" && succeeds(cible())
    ) {
      check_target_vs_population(cible())
    }
    if (!is.null(affected)) {
      showModal(exceeds_population_modal(affected, drawn = TRUE))
    }
  })
  observeEvent(input$seed_value, draw_failed(FALSE), ignoreInit = TRUE)

  observe({
    shinyjs::toggleState(
      "desButton",
      applied() && !draw_failed() && is.null(validate_seed(input$seed_value))
    )
  })

  # Display the results in the UI

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
  dt_style <- "bootstrap5"
  dt_class <- "table table-sm table-striped table-hover nowrap"

  # The four result tables share one look: a filter per column and shorter
  # numbers for the columns the tool adds. These formats are display only: the
  # data, and the CSV files, are untouched.
  result_table <- function(data, height, paging = FALSE) {
    table <- DT::datatable(
      data,
      rownames = FALSE,
      style = dt_style,
      class = dt_class,
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

  quiet <- function(expr) {
    tryCatch(expr, shiny.silent.error = function(e) req(FALSE))
  }

  output$apply_error <- renderUI(
    if (!tables_stale()) {
      validation_alert(
        cible(),
        extra = if (!is.null(issue_list())) {
          div(
            class = "mt-2",
            downloadLink(
              "download_issues",
              tagList(icon("download"), "Download the full list (CSV)")
            )
          )
        }
      )
    }
  )

  output$download_issues <- downloadHandler(
    filename = function() paste0("sampling_issues_", humanTime(), ".csv"),
    content = function(file) {
      write.csv(issue_list(), file, row.names = FALSE, na = "")
    }
  )
  # out() keeps its error until the next draw, so gate it on draw_failed()
  output$sample_error <- renderUI(
    if (draw_failed()) {
      validation_alert(
        out(),
        extra = if (!is.null(draw_issues())) {
          div(
            class = "mt-2",
            downloadLink(
              "download_draw_issues",
              tagList(icon("download"), "Download the full list (CSV)")
            )
          )
        }
      )
    }
  )

  output$download_draw_issues <- downloadHandler(
    filename = function() paste0("draw_issues_", humanTime(), ".csv"),
    content = function(file) {
      write.csv(draw_issues(), file, row.names = FALSE, na = "")
    }
  )

  output$sampling_frame <- DT::renderDataTable({
    req(!tables_stale())
    targets <- quiet(cible())
    result_table(
      quiet(frame()),
      big_table_height(small_table_height(targets)),
      paging = TRUE
    )
  })

  output$target_frame <- DT::renderDataTable({
    req(!tables_stale())
    targets <- label_target_table(quiet(cible()))
    result_table(targets, small_table_height(targets))
  })

  # create the sample based on the sampling frame and the input parameters
  out <- eventReactive(input$desButton, {
    # frame() already validated the strata, cluster and population selections
    # and cible() the cluster size; the seed is checked again because it can
    # change after Apply
    frame()
    require_valid(validate_seed(input$seed_value))
    sample_out <- run_with_seed(
      as.integer(input$seed_value),
      make_sample(db(), input)
    )
    issues <- psu_overflow_issues(sample_out$sample, input)
    draw_issues(issues)
    require_valid(psu_overflow_message(issues))
    sample_out
  })

  # display the results

  # The drawn sample carries every column of the uploaded frame: the sideways
  # scroll keeps long or numerous column names readable.
  output$sampling_output <- DT::renderDataTable({
    req(sample_current())
    result <- quiet(out())
    result_table(
      result[[1]],
      sample_table_height(small_table_height(result[[2]])),
      paging = TRUE
    )
  })

  output$test_1 <- DT::renderDataTable({
    req(sample_current())
    summary <- quiet(out())[[2]]
    result_table(summary, small_table_height(summary))
  })

  # Real download buttons only exist once a sample has been drawn; before that
  # (or after a failed draw) inert placeholders are shown. shinyjs::enable()
  # would leave aria-disabled and tabindex=-1 on the links.
  output$export_buttons <- renderUI({
    ready <- sample_current()
    export_button <- function(id, label, style) {
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
    tagList(
      export_button(
        "downloadBtn2",
        "Sample summary",
        "width: 100%; margin-bottom: 10px;"
      ),
      export_button("downloadBtn", "Sample", "width: 100%;")
    )
  })

  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste("sample_", humanTime(), '.csv', sep = '')
    },
    content = function(file) {
      req(sample_current())
      write.csv(out()[[1]], file, row.names = FALSE)
    }
  )

  output$downloadBtn2 <- downloadHandler(
    filename = function() {
      paste("sampling_summary", humanTime(), '.csv', sep = '')
    },
    content = function(file) {
      req(sample_current())
      write.csv(out()[[2]], file, row.names = FALSE)
    }
  )
}
