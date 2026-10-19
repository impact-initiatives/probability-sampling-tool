# The settings of a draw are kept with its sample, for the output file (issue #60).

test_that("the record keeps the settings, the file, the time and the versions", {
  frame <- data.frame(a = 1:3, b = 4:6)
  at <- as.POSIXct("2026-10-19 13:05:31", tz = "UTC")
  record <- draw_record(
    list(seed = 7),
    "frame.csv",
    frame,
    at,
    list(label = "2026.1.0 (abc1234)")
  )
  expect_identical(record$params, list(seed = 7))
  expect_identical(record$file_name, "frame.csv")
  expect_identical(record$n_rows, 3L)
  expect_identical(record$n_columns, 2L)
  expect_identical(record$drawn_at, at)
  expect_identical(record$app_version, "2026.1.0 (abc1234)")
  expect_identical(record$r_version, R.version.string)
})

test_that("the output file is named after the time of the draw, in UTC", {
  # 15:05:31 in Helsinki is 12:05:31 UTC (summer time, UTC+3, on this date)
  at <- as.POSIXct("2026-10-19 15:05:31", tz = "Europe/Helsinki")
  expect_identical(
    output_file_name(list(drawn_at = at)),
    "PST_Sampling_Output_20261019-120531.xlsx"
  )
  at <- as.POSIXct("2026-01-02 00:00:09", tz = "UTC")
  expect_identical(
    output_file_name(list(drawn_at = at)),
    "PST_Sampling_Output_20260102-000009.xlsx"
  )
})

test_that("a sample that fits in an Excel sheet is accepted", {
  expect_null(excel_size_message(data.frame(a = seq_len(1000))))
  # the header takes a row
  expect_null(excel_size_message(data.frame(a = seq_len(EXCEL_MAX_ROWS - 1))))
})

test_that("a sample too large for an Excel sheet is reported", {
  rows <- excel_size_message(data.frame(a = seq_len(EXCEL_MAX_ROWS)))
  expect_match(rows, "1,048,576 rows", fixed = TRUE)
  expect_match(rows, "at most 1,048,576 rows", fixed = TRUE)
  columns <- excel_size_message(
    as.data.frame(matrix(0, nrow = 1, ncol = EXCEL_MAX_COLUMNS + 1))
  )
  expect_match(columns, "16,385 columns", fixed = TRUE)
})

test_that("a draw keeps the settings it was made with, whatever the form says after", {
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
      seed = 42,
      apply_button = 1
    )
    session$setInputs(draw_button = 1)
    record <- drawn_sample()$record
    expect_identical(record$file_name, "data_example.csv")
    expect_identical(record$n_rows, 860L)
    expect_identical(record$n_columns, 3L)
    expect_identical(record$params$seed, 42)
    expect_identical(record$params$buffer, 0.05)
    expect_s3_class(record$drawn_at, "POSIXct")
    expect_match(record$app_version, paste0("^", APP_VERSION))

    # the form changes after the draw: the record does not
    session$setInputs(buffer = 0.1, seed = 43)
    expect_identical(drawn_sample()$record$params$buffer, 0.05)
    expect_identical(drawn_sample()$record$params$seed, 42)
  })
})
