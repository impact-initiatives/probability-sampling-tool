# This function is the server logic for the Probability Sampling Tool application.
# It defines the reactive behavior and output rendering for the user interface.

function(input, output, session) {
  # give each session a random default seed, visible and editable by the user
  updateNumericInput(session, "seed", value = sample.int(1e6, 1))

  guide_server("guide", session)

  # the settings of the sampling, read from the inputs
  params <- reactive(read_params(input))

  # an uploaded file is either a usable CSV or an error message, never a crash
  uploaded_file <- reactive({
    if (!is.null(input$rejected_file_name)) {
      return(list(
        data = NULL,
        error = csv_format_error(input$rejected_file_name) %||%
          check_message(
            "file_too_large",
            name = input$rejected_file_name,
            limit = MAX_UPLOAD_MB
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
    if (input$data_source == DATA_SOURCE_UPLOAD) uploaded_file()$error
  )

  # get the data
  frame_data <- reactive({
    if (input$data_source == DATA_SOURCE_TEST) {
      data.table::fread("data/data_example.csv")
    } else {
      uploaded_file()$data
    }
  })

  output$file_error <- renderUI({
    if (!is.null(file_error())) {
      div(class = "alert alert-danger py-2 mb-0 mt-2", file_error())
    }
  })

  # one-line feedback under the data source selector
  output$data_status <- renderUI({
    d <- frame_data()
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
      choices = c(NO_COLUMN, colnames(frame_data())),
      selected = NO_COLUMN
    )
    updateSelectInput(
      session,
      "strata_column",
      choices = c(NO_COLUMN, colnames(frame_data())),
      selected = NO_COLUMN
    )
    updateSelectInput(
      session,
      "population_column",
      choices = c(NO_COLUMN, colnames(frame_data())),
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
  apply_issues <- reactiveVal(NULL)
  # same for a draw blocked because a PSU is assigned too many surveys
  draw_issues <- reactiveVal(NULL)
  observeEvent(
    input$apply_button,
    {
      tables_stale(FALSE)
      apply_issues(NULL)
      draw_issues(NULL)
    },
    priority = 10
  )

  # create the sampling frame
  sampling_frame <- eventReactive(input$apply_button, {
    require_valid(validate_strata_selection(params()))
    require_valid(file_error())
    sframe <- frame_data()
    validate(need(!is.null(sframe), check_message("no_file")))
    require_valid(validate_reserved_columns(sframe))
    require_valid(validate_strata_column(sframe, params()))
    require_valid(validate_psu_column(sframe, params()))
    require_valid(validate_population_column(sframe, params()))
    pop_issues <- population_issues(sframe, params())
    apply_issues(pop_issues)
    require_valid(population_message(pop_issues, params()$population_column))
    require_valid(validate_parameters(params()))
    # not needed for the frame, but reported here rather than at the draw
    require_valid(validate_seed(input$seed))
    format_sampling_frame(sframe, params())
  })

  # create the target sampling size by strata
  targets <- eventReactive(input$apply_button, {
    target_data <- create_targets(sampling_frame(), params())
    issues <- cluster_size_issues(sampling_frame(), target_data, params())
    apply_issues(issues)
    require_valid(cluster_size_message(issues, params()$cluster_size))
    target_data
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
    if (!succeeds(sampling_frame()) || !succeeds(targets())) {
      return()
    }
    applied(TRUE)

    # cluster sampling never caps the draw: targets() already blocks a target
    # above the room of a stratum
    affected <- if (params()$sampling_type != SAMPLING_TYPE_CLUSTER) {
      check_target_vs_population(targets())
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
    drawn <- succeeds(drawn_sample())
    sample_current(drawn)
    if (!drawn) {
      draw_failed(TRUE)
      return()
    }
    # same warning as at Apply: the draw has just been capped
    affected <- if (
      params()$sampling_type != SAMPLING_TYPE_CLUSTER &&
        succeeds(targets())
    ) {
      check_target_vs_population(targets())
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
        targets(),
        extra = if (!is.null(apply_issues())) {
          issues_download_link("download_issues")
        }
      )
    }
  )

  output$download_issues <- downloadHandler(
    filename = function() paste0("sampling_issues_", humanTime(), ".csv"),
    content = function(file) {
      write.csv(apply_issues(), file, row.names = FALSE, na = "")
    }
  )
  # drawn_sample() keeps its error until the next draw, so gate it on draw_failed()
  output$sample_error <- renderUI(
    if (draw_failed()) {
      validation_alert(
        drawn_sample(),
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

  output$frame_table <- DT::renderDataTable({
    req(!tables_stale())
    target_data <- quiet(targets())
    result_table(
      quiet(sampling_frame()),
      big_table_height(small_table_height(target_data)),
      paging = TRUE
    )
  })

  output$target_table <- DT::renderDataTable({
    req(!tables_stale())
    target_data <- label_target_table(quiet(targets()))
    result_table(target_data, small_table_height(target_data))
  })

  # create the sample based on the sampling frame and the input parameters
  drawn_sample <- eventReactive(input$draw_button, {
    # sampling_frame() already validated the strata, cluster and population selections
    # and targets() the cluster size; the seed is checked again because it can
    # change after Apply
    sampling_frame()
    require_valid(validate_seed(input$seed))
    sample_out <- run_with_seed(
      as.integer(params()$seed),
      make_sample(frame_data(), params())
    )
    issues <- psu_overflow_issues(sample_out$sample, params())
    draw_issues(issues)
    require_valid(psu_overflow_message(issues))
    sample_out
  })

  # display the results

  # The drawn sample carries every column of the uploaded frame: the sideways
  # scroll keeps long or numerous column names readable.
  output$sample_table <- DT::renderDataTable({
    req(sample_current())
    result <- quiet(drawn_sample())
    result_table(
      result[[1]],
      sample_table_height(small_table_height(result[[2]])),
      paging = TRUE
    )
  })

  output$summary_table <- DT::renderDataTable({
    req(sample_current())
    summary <- quiet(drawn_sample())[[2]]
    result_table(summary, small_table_height(summary))
  })

  output$export_buttons <- renderUI({
    ready <- sample_current()
    tagList(
      export_button(
        "download_summary",
        "Sample summary",
        "width: 100%; margin-bottom: 10px;",
        ready
      ),
      export_button("download_sample", "Sample", "width: 100%;", ready)
    )
  })

  output$download_sample <- downloadHandler(
    filename = function() {
      paste("sample_", humanTime(), '.csv', sep = '')
    },
    content = function(file) {
      req(sample_current())
      write.csv(drawn_sample()[[1]], file, row.names = FALSE)
    }
  )

  output$download_summary <- downloadHandler(
    filename = function() {
      paste("sampling_summary", humanTime(), '.csv', sep = '')
    },
    content = function(file) {
      req(sample_current())
      write.csv(drawn_sample()[[2]], file, row.names = FALSE)
    }
  )
}
