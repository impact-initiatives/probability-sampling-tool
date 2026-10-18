# The sample on screen is the one drawn after the last Apply (issue #33).
# Logic only, no browser: see test-sample-lifecycle-app.R for the real page.

test_that("a new Apply hides the previous sample and its downloads", {
  withr::local_dir(app_root)
  server <- source("server.R")$value

  hidden <- function(expr) expect_error(expr, class = "shiny.silent.error")

  shiny::testServer(server, {
    session$setInputs(
      data_source = "test",
      sampling_mode = "Sample size based on population",
      sampling_type = "Simple random sampling",
      stratified = "Not stratified",
      psu_column = "None",
      strata_column = "None",
      population_column = "None",
      confidence_level = 0.95,
      error_margin = 0.05,
      proportion = 0.5,
      buffer = 0.05,
      target_size = NA,
      cluster_size = 5,
      icc = 0.06,
      seed = 42
    )
    expect_false(sample_current())

    session$setInputs(apply_button = 1)
    expect_true(applied())
    hidden(output$sampling_output)

    session$setInputs(draw_button = 1)
    expect_true(sample_current())
    expect_no_error(output$sampling_output)
    expect_no_error(output$test_1)
    expect_no_error(output$downloadBtn)
    expect_no_error(output$downloadBtn2)

    # changing a parameter without Apply keeps the last validated sample
    session$setInputs(buffer = 0.10)
    expect_false(applied())
    expect_true(sample_current())

    session$setInputs(apply_button = 2)
    expect_false(sample_current())
    hidden(output$sampling_output)
    hidden(output$test_1)
    hidden(output$downloadBtn)
    hidden(output$downloadBtn2)

    session$setInputs(draw_button = 2)
    expect_true(sample_current())
    expect_no_error(output$sampling_output)

    # an Apply that fails validation hides it too
    session$setInputs(stratified = "Stratified", apply_button = 3)
    expect_false(applied())
    expect_false(sample_current())
    hidden(output$downloadBtn)
  })
})
