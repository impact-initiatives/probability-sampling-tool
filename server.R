# This function is the server logic for the Probability Sampling Tool application.
# It defines the reactive behavior and output rendering for the user interface.

function(input, output, session) {
  # get the data
  db <- reactive({
    inFile <- input$popdata
    if (input$testdata == TRUE) {
      fread("data_example.csv")
    } else if (is.null(inFile)) {
      return(NULL)
    } else {
      fread(inFile$datapath)
    }
  })

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

  # create the sampling frame
  frame <- eventReactive(input$f_apply, {
    strata_msg <- validate_strata_selection(input)
    validate(need(is.null(strata_msg), strata_msg))
    format_sampling_frame(db(), input)
  })

  # create the target sampling size by strata
  cible <- eventReactive(input$f_apply, {
    create_targets(frame(), input)
  })

  # enable "Sample!" only once "Apply" has produced a valid sampling frame and target
  observeEvent(input$f_apply, {
    frame()
    cible()
    shinyjs::enable("desButton")

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

  # any change to a frame-defining input invalidates the previous Apply
  observeEvent(
    list(
      input$popdata,
      input$testdata,
      input$samp_type,
      input$stratified,
      input$col_psu,
      input$strata,
      input$colpop
    ),
    shinyjs::disable("desButton"),
    ignoreInit = TRUE
  )
  # Display the results in the UI
  output$sampling_frame <- DT::renderDataTable(
    frame(),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )

  output$target_frame <- DT::renderDataTable(
    cible(),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = F)
  )

  # create the sample based on the sampling frame and the input parameters
  out <- eventReactive(input$desButton, {
    strata_msg <- validate_strata_selection(input)
    validate(need(is.null(strata_msg), strata_msg))
    validate(need(
      is.null(validate_cluster_size(frame(), input)),
      validate_cluster_size(frame(), input)
    ))
    make_sample(db(), input)
  })

  # display the results

  output$sampling_output <- DT::renderDataTable(
    out()[[1]],
    rownames = FALSE,
    options = list(searching = FALSE)
  )

  output$test_1 <- DT::renderDataTable(
    out()[[2]],
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )

  observeEvent(input$refresh, {
    output$test_1 <- DT::renderDataTable(
      out()[[2]],
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    )
  })

  observeEvent(input$refresh, {
    output$sampling_output <- DT::renderDataTable(
      out()[[1]],
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
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
