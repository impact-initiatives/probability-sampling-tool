# The sample on screen is the one drawn after the last Apply (issue #33).
# Logic only, no browser: see test-sample-lifecycle-app.R for the real page.

app_root <- normalizePath(testthat::test_path("..", ".."))

test_that("a new Apply hides the previous sample and its downloads", {
  withr::local_dir(app_root)
  source("global.R")
  server <- source("server.R")$value

  hidden <- function(expr) expect_error(expr, class = "shiny.silent.error")

  shiny::testServer(server, {
    session$setInputs(
      data_source = "test",
      topup = "Sample size based on population",
      samp_type = "Simple random sampling",
      stratified = "Not stratified",
      col_psu = "None",
      strata = "None",
      colpop = "None",
      conf_level = 0.95,
      e_marg = 0.05,
      pror = 0.5,
      buf = 0.05,
      target = NA,
      cls = 5,
      ICC = 0.06,
      seed_value = 42
    )
    expect_false(sample_current())

    session$setInputs(f_apply = 1)
    expect_true(applied())
    hidden(output$sampling_output)

    session$setInputs(desButton = 1)
    expect_true(sample_current())
    expect_no_error(output$sampling_output)
    expect_no_error(output$test_1)
    expect_no_error(output$downloadBtn)
    expect_no_error(output$downloadBtn2)

    # changing a parameter without Apply keeps the last validated sample
    session$setInputs(buf = 0.10)
    expect_false(applied())
    expect_true(sample_current())

    session$setInputs(f_apply = 2)
    expect_false(sample_current())
    hidden(output$sampling_output)
    hidden(output$test_1)
    hidden(output$downloadBtn)
    hidden(output$downloadBtn2)

    session$setInputs(desButton = 2)
    expect_true(sample_current())
    expect_no_error(output$sampling_output)

    # an Apply that fails validation hides it too
    session$setInputs(stratified = "Stratified", f_apply = 3)
    expect_false(applied())
    expect_false(sample_current())
    hidden(output$downloadBtn)
  })
})
