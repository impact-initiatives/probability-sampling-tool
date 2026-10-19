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
      stratified = FALSE,
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
    hidden(output$sample_table)

    session$setInputs(draw_button = 1)
    expect_true(sample_current())
    expect_no_error(output$sample_table)
    expect_no_error(output$summary_table)
    expect_no_error(output$download_sample)
    expect_no_error(output$download_summary)

    # changing a parameter without Apply keeps the last validated sample
    session$setInputs(buffer = 0.10)
    expect_false(applied())
    expect_true(sample_current())

    session$setInputs(apply_button = 2)
    expect_false(sample_current())
    hidden(output$sample_table)
    hidden(output$summary_table)
    hidden(output$download_sample)
    hidden(output$download_summary)

    session$setInputs(draw_button = 2)
    expect_true(sample_current())
    expect_no_error(output$sample_table)

    # an Apply that fails validation hides it too
    session$setInputs(stratified = TRUE, apply_button = 3)
    expect_false(applied())
    expect_false(sample_current())
    hidden(output$download_sample)
  })
})
