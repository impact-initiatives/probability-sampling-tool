# This function is the server logic for the Probability Sampling Tool application.
# It defines the reactive behavior and output rendering for the user interface.

function(input, output, session) {
  # give each session a random default seed, visible and editable by the user
  updateNumericInput(session, "seed_value", value = sample.int(1e6, 1))

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
  observeEvent(input$f_apply, tables_stale(FALSE), priority = 10)

  # create the sampling frame
  frame <- eventReactive(input$f_apply, {
    require_valid(validate_strata_selection(input))
    require_valid(file_error())
    sframe <- db()
    validate(need(
      !is.null(sframe),
      "Please upload a CSV file before applying."
    ))
    require_valid(validate_psu_column(sframe, input))
    require_valid(validate_population_column(sframe, input))
    require_valid(validate_parameters(input))
    # not needed for the frame, but reported here rather than at the draw
    require_valid(validate_seed(input$seed_value))
    format_sampling_frame(sframe, input)
  })

  # create the target sampling size by strata
  cible <- eventReactive(input$f_apply, {
    create_targets(frame(), input)
  })

  # "Sample" is usable after a successful Apply, until a parameter changes or a
  # draw fails; an impossible seed blocks it too.
  applied <- reactiveVal(FALSE)
  draw_failed <- reactiveVal(FALSE)

  observeEvent(input$f_apply, {
    applied(FALSE)
    draw_failed(FALSE)
    # the errors are shown by the outputs; here they must not end the session
    if (!succeeds(frame()) || !succeeds(cible())) {
      return()
    }
    applied(TRUE)

    affected <- check_target_vs_population(cible())
    if (!is.null(affected)) {
      showModal(modalDialog(
        title = tagList(
          icon("triangle-exclamation"),
          "Sample size exceeds population"
        ),
        div(
          class = "alert alert-warning",
          paste0(
            "The requested sample size exceeds the available population in stratum(s): ",
            paste(affected, collapse = ", "),
            ". The sample will be capped to the available population there."
          )
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
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
      input$pror,
      input$e_marg,
      input$buf,
      input$cls,
      input$ICC
    ),
    applied(FALSE),
    ignoreInit = TRUE
  )

  observeEvent(input$desButton, {
    if (!succeeds(out())) draw_failed(TRUE)
  })
  observeEvent(input$seed_value, draw_failed(FALSE), ignoreInit = TRUE)

  observe({
    shinyjs::toggleState(
      "desButton",
      applied() && !draw_failed() && is.null(validate_seed(input$seed_value))
    )
  })

  # Display the results in the UI

  # Body height of the large tables: what is left under the small table above.
  big_table_height <- "max(120px, 100vh - 605px)"
  # 584px = the rest of the Sampling tab, so the page fits whatever the height
  sample_table_height <- "max(80px, 100vh - 14vh - 584px)"
  small_table_height <- "14vh"

  # Bootstrap 5 table look; the wide tables also keep their cells on one line.
  dt_style <- "bootstrap5"
  dt_class <- "table table-sm table-striped table-hover"
  dt_class_wide <- paste(dt_class, "nowrap")

  # A failed validation is shown once, as an alert, instead of inside each table.
  validation_alert <- function(expr) {
    msg <- validation_message(expr)
    if (length(msg) && nzchar(msg)) div(class = "alert alert-danger", msg)
  }

  quiet <- function(expr) {
    tryCatch(expr, shiny.silent.error = function(e) req(FALSE))
  }

  output$apply_error <- renderUI(
    if (!tables_stale()) validation_alert(cible())
  )
  # out() keeps its error until the next draw, so gate it on draw_failed()
  output$sample_error <- renderUI(if (draw_failed()) validation_alert(out()))

  output$sampling_frame <- DT::renderDataTable(
    {
      req(!tables_stale())
      quiet(frame())
    },
    rownames = FALSE,
    style = dt_style,
    class = dt_class_wide,
    options = list(
      searching = FALSE,
      lengthChange = FALSE,
      pageLength = 100,
      scrollY = big_table_height,
      scrollCollapse = TRUE,
      scrollX = TRUE
    )
  )

  output$target_frame <- DT::renderDataTable(
    {
      req(!tables_stale())
      target <- quiet(cible())
      # target_srs is an internal SRS-equivalent used by the cluster
      # sampling fallback, not meant for display
      target[, setdiff(names(target), "target_srs")]
    },
    rownames = FALSE,
    style = dt_style,
    class = dt_class,
    options = list(
      searching = FALSE,
      lengthChange = F,
      scrollY = small_table_height,
      scrollCollapse = TRUE,
      scrollX = TRUE
    )
  )

  # create the sample based on the sampling frame and the input parameters
  out <- eventReactive(input$desButton, {
    # frame() already validated the strata, cluster and population selections;
    # the seed is checked again because it can change after Apply
    frame()
    require_valid(validate_cluster_size(frame(), input))
    require_valid(validate_seed(input$seed_value))
    run_with_seed(as.integer(input$seed_value), make_sample(db(), input))
  })

  # display the results

  # The drawn sample carries every column of the uploaded frame: scrollX keeps
  # long or numerous column names readable.
  render_sample <- function() {
    DT::renderDataTable(
      quiet(out())[[1]],
      rownames = FALSE,
      style = dt_style,
      class = dt_class_wide,
      options = list(
        searching = FALSE,
        lengthChange = FALSE,
        pageLength = 100,
        scrollY = sample_table_height,
        scrollCollapse = TRUE,
        scrollX = TRUE
      )
    )
  }

  render_summary <- function() {
    DT::renderDataTable(
      quiet(out())[[2]],
      rownames = FALSE,
      style = dt_style,
      class = dt_class,
      options = list(
        searching = FALSE,
        lengthChange = FALSE,
        scrollY = small_table_height,
        scrollCollapse = TRUE,
        scrollX = TRUE
      )
    )
  }

  output$sampling_output <- render_sample()
  output$test_1 <- render_summary()

  # Real download buttons only exist once a sample has been drawn; before that
  # (or after a failed draw) inert placeholders are shown. shinyjs::enable()
  # would leave aria-disabled and tabindex=-1 on the links.
  output$export_buttons <- renderUI({
    ready <- succeeds(out())
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
      write.csv(out()[[1]], file)
    }
  )

  output$downloadBtn2 <- downloadHandler(
    filename = function() {
      paste("sampling_summary", humanTime(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(out()[[2]], file)
    }
  )
}
