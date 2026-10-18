# This function is the server logic for the Probability Sampling Tool application.
# It defines the reactive behavior and output rendering for the user interface.

function(input, output, session) {
  # give each session a random default seed, visible and editable by the user
  updateNumericInput(session, "seed", value = sample.int(1e6, 1))

  guide_server("guide", session)

  # the settings of the sampling, read from the inputs
  params <- reactive(read_params(input))

  # an uploaded file is either a usable CSV or an error message, never a crash
  upload <- reactive({
    if (!is.null(input$rejected_file_name)) {
      return(list(
        data = NULL,
        error = csv_format_error(input$rejected_file_name) %||%
          paste0(
            "'",
            input$rejected_file_name,
            "' is larger than the ",
            MAX_UPLOAD_MB,
            " MB limit."
          )
      ))
    }
    f <- input$frame_file
    if (is.null(f)) {
      return(NULL)
    }
    read_uploaded_csv(f$name, f$datapath)
  })

  file_error <- reactive(
    if (input$data_source == DATA_SOURCE_UPLOAD) upload()$error
  )

  # get the data
  db <- reactive({
    if (input$data_source == DATA_SOURCE_TEST) {
      fread("data/data_example.csv")
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
    src <- if (input$data_source == DATA_SOURCE_TEST) {
      "data_example.csv"
    } else {
      input$frame_file$name
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
    content = function(file) file.copy("data/data_example.csv", file)
  )

  # update the check ox based on the data headings
  observe({
    updateSelectInput(
      session,
      "psu_column",
      choices = c(NO_COLUMN, colnames(db())),
      selected = NO_COLUMN
    )
    updateSelectInput(
      session,
      "strata_column",
      choices = c(NO_COLUMN, colnames(db())),
      selected = NO_COLUMN
    )
    updateSelectInput(
      session,
      "population_column",
      choices = c(NO_COLUMN, colnames(db())),
      selected = NO_COLUMN
    )
  })

  # a new upload hides the frame and target tables until the next Apply
  tables_stale <- reactiveVal(FALSE)
  observeEvent(
    list(input$frame_file, input$rejected_file_name),
    tables_stale(TRUE),
    ignoreInit = TRUE
  )
  # what Apply found wrong in the data, for the downloadable list
  issue_list <- reactiveVal(NULL)
  # same for a draw blocked because a PSU is assigned too many surveys
  draw_issues <- reactiveVal(NULL)
  observeEvent(
    input$apply_button,
    {
      tables_stale(FALSE)
      issue_list(NULL)
      draw_issues(NULL)
    },
    priority = 10
  )

  # create the sampling frame
  frame <- eventReactive(input$apply_button, {
    require_valid(validate_strata_selection(params()))
    require_valid(file_error())
    sframe <- db()
    validate(need(
      !is.null(sframe),
      "Please upload a CSV file before applying."
    ))
    require_valid(validate_reserved_columns(sframe))
    require_valid(validate_psu_column(sframe, params()))
    require_valid(validate_population_column(sframe, params()))
    pop_issues <- population_issues(sframe, params())
    issue_list(pop_issues)
    require_valid(population_message(pop_issues, params()$population_column))
    require_valid(validate_parameters(params()))
    # not needed for the frame, but reported here rather than at the draw
    require_valid(validate_seed(input$seed))
    format_sampling_frame(sframe, params())
  })

  # create the target sampling size by strata
  cible <- eventReactive(input$apply_button, {
    targets <- create_targets(frame(), params())
    issues <- cluster_size_issues(frame(), targets, params())
    issue_list(issues)
    require_valid(cluster_size_message(issues, params()$cluster_size))
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
  observeEvent(input$apply_button, sample_current(FALSE), priority = 10)

  observeEvent(input$apply_button, {
    applied(FALSE)
    draw_failed(FALSE)
    # the errors are shown by the outputs; here they must not end the session
    if (!succeeds(frame()) || !succeeds(cible())) {
      return()
    }
    applied(TRUE)

    # cluster sampling never caps the draw: cible() already blocks a target
    # above the room of a stratum
    affected <- if (params()$sampling_type != SAMPLING_TYPE_CLUSTER) {
      check_target_vs_population(cible())
    }
    if (!is.null(affected)) showModal(exceeds_population_modal(affected))
  })

  # any change to a frame- or target-defining input invalidates the previous Apply
  observeEvent(
    list(
      input$frame_file,
      input$rejected_file_name,
      input$data_source,
      input$sampling_type,
      input$stratified,
      input$psu_column,
      input$strata_column,
      input$population_column,
      input$sampling_mode,
      input$target_size,
      input$confidence_level,
      input$error_margin,
      input$buffer,
      input$cluster_size,
      input$icc
    ),
    applied(FALSE),
    ignoreInit = TRUE
  )

  observeEvent(input$draw_button, {
    drawn <- succeeds(out())
    sample_current(drawn)
    if (!drawn) {
      draw_failed(TRUE)
      return()
    }
    # same warning as at Apply: the draw has just been capped
    affected <- if (
      params()$sampling_type != SAMPLING_TYPE_CLUSTER &&
        succeeds(cible())
    ) {
      check_target_vs_population(cible())
    }
    if (!is.null(affected)) {
      showModal(exceeds_population_modal(affected, drawn = TRUE))
    }
  })
  observeEvent(input$seed, draw_failed(FALSE), ignoreInit = TRUE)

  observe({
    shinyjs::toggleState(
      "draw_button",
      applied() && !draw_failed() && is.null(validate_seed(input$seed))
    )
  })

  # Display the results in the UI

  output$apply_error <- renderUI(
    if (!tables_stale()) {
      validation_alert(
        cible(),
        extra = if (!is.null(issue_list())) {
          issues_download_link("download_issues")
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
          issues_download_link("download_draw_issues")
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
  out <- eventReactive(input$draw_button, {
    # frame() already validated the strata, cluster and population selections
    # and cible() the cluster size; the seed is checked again because it can
    # change after Apply
    frame()
    require_valid(validate_seed(input$seed))
    sample_out <- run_with_seed(
      as.integer(params()$seed),
      make_sample(db(), params())
    )
    issues <- psu_overflow_issues(sample_out$sample, params())
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

  output$export_buttons <- renderUI({
    ready <- sample_current()
    tagList(
      export_button(
        "downloadBtn2",
        "Sample summary",
        "width: 100%; margin-bottom: 10px;",
        ready
      ),
      export_button("downloadBtn", "Sample", "width: 100%;", ready)
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
