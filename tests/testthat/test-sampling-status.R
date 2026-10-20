# The Sampling tab says why "Sample" and the downloads are or are not usable
# (issue #60): one state at a time, the first that applies.

status_id <- function(
  apply_clicked = TRUE,
  setup_changed = FALSE,
  applied = TRUE,
  seed_message = NULL,
  draw_failed = FALSE,
  sample_current = FALSE
) {
  sampling_status(
    apply_clicked,
    setup_changed,
    applied,
    seed_message,
    draw_failed,
    sample_current
  )$id
}

test_that("each state of the sampling has its own message", {
  expect_identical(status_id(FALSE, applied = FALSE), "not_applied")
  expect_identical(
    status_id(setup_changed = TRUE, applied = FALSE),
    "setup_changed"
  )
  expect_identical(status_id(applied = FALSE), "apply_failed")
  expect_identical(status_id(seed_message = "Seed must be..."), "seed_invalid")
  expect_identical(status_id(draw_failed = TRUE), "draw_blocked")
  expect_identical(status_id(sample_current = TRUE), "sampled")
  expect_identical(status_id(), "ready")
})

test_that("the first state that applies wins", {
  # nothing applied beats everything
  expect_identical(
    status_id(FALSE, setup_changed = TRUE, applied = FALSE, draw_failed = TRUE),
    "not_applied"
  )
  # a change after a failed Apply: apply again
  expect_identical(
    status_id(setup_changed = TRUE, applied = FALSE, draw_failed = TRUE),
    "setup_changed"
  )
  # an invalid seed beats a blocked draw and a sample on screen
  expect_identical(
    status_id(
      seed_message = "x",
      draw_failed = TRUE,
      sample_current = TRUE
    ),
    "seed_invalid"
  )
  expect_identical(
    status_id(draw_failed = TRUE, sample_current = TRUE),
    "draw_blocked"
  )
})

test_that("a changed setup says the previous sample is hidden", {
  status <- sampling_status(TRUE, TRUE, FALSE, NULL, FALSE, FALSE)
  expect_match(status$text, "previous sample is hidden")
})

test_that("every message says what to do and where", {
  for (args in list(
    list(FALSE, FALSE, FALSE, NULL, FALSE, FALSE),
    list(TRUE, TRUE, FALSE, NULL, FALSE, FALSE),
    list(TRUE, FALSE, FALSE, NULL, FALSE, FALSE),
    list(TRUE, FALSE, TRUE, "Seed must be...", FALSE, FALSE),
    list(TRUE, FALSE, TRUE, NULL, TRUE, FALSE),
    list(TRUE, FALSE, TRUE, NULL, FALSE, TRUE),
    list(TRUE, FALSE, TRUE, NULL, FALSE, FALSE)
  )) {
    status <- do.call(sampling_status, args)
    expect_true(status$kind %in% c("info", "warning", "danger", "success"))
    expect_true(nzchar(status$text))
  }
})

test_that("the tables say why they are empty, and the downloads why they are inert", {
  withr::local_dir(app_root)
  server <- source("server.R")$value
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
    expect_identical(status()$id, "not_applied")
    expect_match(output$sample_status$html, "Nothing is applied yet")
    expect_match(output$summary_empty$html, "No sample to show")
    expect_match(output$export_buttons$html, "Nothing is applied yet")

    session$setInputs(apply_button = 1)
    expect_identical(status()$id, "ready")
    expect_match(output$sample_empty$html, "Click Sample")

    session$setInputs(draw_button = 1)
    expect_identical(status()$id, "sampled")
    expect_null(output$sample_empty$html)
    expect_match(output$export_buttons$html, 'id="download_output"')
    expect_no_match(output$export_buttons$html, "<button")

    # a change without Apply hides the sample and inactivates the downloads
    session$setInputs(buffer = 0.1)
    expect_identical(status()$id, "setup_changed")
    expect_match(output$sample_status$html, "previous sample is hidden")
    expect_match(output$sample_empty$html, "No sample to show")
    expect_match(output$export_buttons$html, "<button")
    expect_match(output$export_buttons$html, "setup changed")

    session$setInputs(apply_button = 2)
    expect_identical(status()$id, "ready")
    expect_match(output$export_buttons$html, "Click Sample")

    # an Apply that fails
    session$setInputs(stratified = TRUE, apply_button = 3)
    expect_identical(status()$id, "apply_failed")
    # and a change after it: apply again
    session$setInputs(strata_column = "stratum")
    expect_identical(status()$id, "setup_changed")

    # an invalid seed
    session$setInputs(stratified = FALSE, apply_button = 4)
    expect_identical(status()$id, "ready")
    session$setInputs(seed = NA)
    expect_identical(status()$id, "seed_invalid")
    expect_match(output$sample_status$html, "Seed must be")
  })
})
