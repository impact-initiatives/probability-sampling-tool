# The output file: three sheets, the inputs of the draw, the summary and the sample
# (issue #60). The workbook is saved and read back, as a user opens it.

frame <- data.table::fread(file.path(app_root, EXAMPLE_DATA_PATH))

settings <- function(...) {
  base <- list(
    sampling_mode = SAMPLING_MODE_FROM_POPULATION,
    sampling_type = SAMPLING_TYPE_CLUSTER,
    stratified = TRUE,
    strata_column = "stratum",
    psu_column = "psu_id",
    population_column = "population",
    confidence_level = 0.95,
    error_margin = 0.05,
    proportion = 0.5,
    buffer = 0.05,
    target_size = 300,
    cluster_size = 5,
    icc = 0.06,
    seed = 42
  )
  normalize_params(utils::modifyList(base, list(...)))
}

at <- as.POSIXct("2026-10-19 13:05:31", tz = "UTC")

draw <- function(params) {
  out <- run_with_seed(42, make_sample(frame, params))
  record <- draw_record(
    params,
    "data_example.csv",
    frame,
    at,
    list(label = "2026.1.0 (abc1234)")
  )
  list(out = out, record = record)
}

saved <- function(d) {
  wb <- build_output_workbook(d$out$sample, d$out$summary_sample, d$record)
  path <- withr::local_tempfile(
    fileext = ".xlsx",
    .local_envir = parent.frame()
  )
  wb$save(path)
  list(wb = openxlsx2::wb_load(path), path = path)
}

# the XML of a sheet of the saved file, which is what a spreadsheet program reads
sheet_xml <- function(path, i) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  utils::unzip(
    path,
    files = sprintf("xl/worksheets/sheet%d.xml", i),
    exdir = dir
  )
  paste(
    readLines(
      file.path(dir, "xl", "worksheets", sprintf("sheet%d.xml", i)),
      warn = FALSE
    ),
    collapse = ""
  )
}

settings_of <- function(record) {
  vapply(input_rows(record), function(line) line$setting, character(1))
}

test_that("the workbook has the three sheets, in order, named in snake_case", {
  file <- saved(draw(settings()))
  expect_identical(
    unname(openxlsx2::wb_get_sheet_names(file$wb)),
    c("inputs", "sample_summary", "sample")
  )
  # no space, no digit
  expect_match(unname(OUTPUT_SHEETS), "^[a-z_]+$")
})

test_that("the summary and the sample sheets hold the tables of the app", {
  d <- draw(settings())
  file <- saved(d)
  summary <- openxlsx2::wb_to_df(file$path, sheet = "sample_summary")
  expect_identical(names(summary), names(d$out$summary_sample))
  expect_equal(nrow(summary), nrow(d$out$summary_sample))
  expect_equal(summary$Surveys, d$out$summary_sample$Surveys)
  expect_equal(summary$Seed, d$out$summary_sample$Seed)

  sample <- openxlsx2::wb_to_df(file$path, sheet = "sample")
  expect_identical(names(sample), names(d$out$sample))
  expect_equal(nrow(sample), nrow(d$out$sample))
  expect_identical(sample$PST_id_sample, d$out$sample$PST_id_sample)
  expect_equal(sample$PST_survey, d$out$sample$PST_survey)
  expect_equal(sample$PST_prob, d$out$sample$PST_prob)
  # a factor of the sample is read back as its text
  expect_identical(
    as.character(sample$PST_psu_id),
    as.character(d$out$sample$PST_psu_id)
  )
})

test_that("an empty value is an empty cell, and a text that looks like a formula stays text", {
  wb <- openxlsx2::wb_workbook()
  add_table_sheet(
    wb,
    "t",
    data.frame(a = c(1.5, NA), b = c("=SUM(A1)", NA), stringsAsFactors = FALSE)
  )
  path <- withr::local_tempfile(fileext = ".xlsx")
  wb$save(path)
  read <- openxlsx2::wb_to_df(path, sheet = "t")
  expect_true(is.na(read$a[2]))
  expect_true(is.na(read$b[2]))
  expect_identical(read$b[1], "=SUM(A1)")
  cells <- openxlsx2::wb_load(path)$worksheets[[1]]$sheet_data$cc
  expect_true(all(is.na(cells$f) | cells$f == ""))
})

test_that("the table sheets have a frozen header, a filter and readable column widths", {
  file <- saved(draw(settings()))
  for (i in 2:3) {
    xml <- sheet_xml(file$path, i)
    expect_match(xml, 'state="frozen"', fixed = TRUE)
    expect_match(xml, "<autoFilter ref=\"A1:", fixed = TRUE)
    widths <- as.numeric(regmatches(
      xml,
      gregexpr('(?<= width=")[0-9.]+', xml, perl = TRUE)
    )[[1]])
    expect_true(all(widths >= 8 & widths <= 46))
  }
  d <- draw(settings())
  widths <- table_column_widths(d$out$sample)
  expect_length(widths, ncol(d$out$sample))
  expect_true(all(widths >= 8 & widths <= 45))
  # a header longer than its values sets the width
  expect_gte(
    table_column_widths(data.frame(a_long_header_name = 1))[1],
    nchar("a_long_header_name")
  )
})

test_that("the inputs sheet has sections, wide columns, no grid lines and a closing sentence", {
  file <- saved(draw(settings()))
  inputs <- openxlsx2::wb_to_df(file$path, sheet = "inputs", col_names = FALSE)
  first_column <- inputs[[1]]
  expect_identical(
    first_column[!is.na(first_column)][1],
    "Probability sampling tool: inputs of the draw"
  )
  for (section in c("Traceability", "Data source", "Sampling", "Parameters")) {
    expect_true(section %in% first_column, info = section)
  }
  expect_match(
    first_column[length(first_column)],
    "same file, the same settings, the same seed"
  )
  xml <- sheet_xml(file$path, 1)
  expect_match(xml, 'showGridLines="0"', fixed = TRUE)
  expect_match(xml, "<mergeCell ref=\"A[0-9]+:B[0-9]+\"/>")
  expect_match(xml, 'customWidth="1"', fixed = TRUE)
})

test_that("the inputs sheet gives the traceability of the draw", {
  d <- draw(settings())
  lines <- stats::setNames(
    lapply(input_rows(d$record), function(line) line$value),
    settings_of(d$record)
  )
  expect_identical(lines[["App version"]], "2026.1.0 (abc1234)")
  expect_identical(lines[["R version"]], R.version.string)
  expect_identical(
    lines[["Date and time of the draw (UTC)"]],
    "2026-10-19 13:05:31"
  )
  expect_identical(lines[["File"]], "data_example.csv")
  expect_identical(lines[["Rows"]], 860L)
  expect_identical(lines[["Columns"]], 3L)
  expect_identical(lines[["Seed"]], 42L)
})

test_that("the inputs sheet gives the settings of the draw, not those of the form", {
  d <- draw(settings(seed = 42, buffer = 0.05))
  # the form changes after the draw: the record is the one of the draw
  record <- d$record
  later <- settings(seed = 99, buffer = 0.9)
  expect_false(identical(later$seed, record$params$seed))
  lines <- stats::setNames(
    lapply(input_rows(record), function(line) line$value),
    settings_of(record)
  )
  expect_identical(lines[["Seed"]], 42L)
  expect_identical(lines[["Buffer"]], 0.05)
})

test_that("only the settings of the mode and the type of the draw are listed", {
  expected <- list(
    cluster_population = c(
      "Mode of sampling",
      "Type of sampling",
      "Stratified",
      "Input stratum",
      "Input cluster",
      "Input population",
      "Confidence level",
      "Error margin",
      "Proportion",
      "Buffer",
      "Cluster size",
      "ICC",
      "Seed"
    ),
    # the ICC and the cluster size are listed with "Enter sample size" too
    cluster_entered = c(
      "Mode of sampling",
      "Type of sampling",
      "Stratified",
      "Input cluster",
      "Input population",
      "Enter target sample size",
      "Cluster size",
      "ICC",
      "Seed"
    ),
    pps_population = c(
      "Mode of sampling",
      "Type of sampling",
      "Stratified",
      "Input population",
      "Confidence level",
      "Error margin",
      "Proportion",
      "Buffer",
      "Seed"
    ),
    simple_entered = c(
      "Mode of sampling",
      "Type of sampling",
      "Stratified",
      "Enter target sample size",
      "Seed"
    )
  )
  cases <- list(
    cluster_population = settings(),
    cluster_entered = settings(
      sampling_mode = SAMPLING_MODE_ENTER_SIZE,
      stratified = FALSE
    ),
    pps_population = settings(
      sampling_type = SAMPLING_TYPE_PPS,
      stratified = FALSE
    ),
    simple_entered = settings(
      sampling_mode = SAMPLING_MODE_ENTER_SIZE,
      sampling_type = SAMPLING_TYPE_SIMPLE,
      stratified = FALSE
    )
  )
  for (name in names(cases)) {
    record <- draw(cases[[name]])$record
    listed <- settings_of(record)
    parameters <- listed[
      !listed %in%
        c(
          "App version",
          "R version",
          "Date and time of the draw (UTC)",
          "File",
          "Rows",
          "Columns"
        )
    ]
    expect_identical(parameters, expected[[name]], info = name)
  }
})

test_that("a number of the inputs is a number in the sheet, a text is a text", {
  file <- saved(draw(settings()))
  inputs <- openxlsx2::wb_to_df(file$path, sheet = "inputs", col_names = FALSE)
  cells <- file$wb$worksheets[[1]]$sheet_data$cc
  value_cell <- function(label) {
    row <- rownames(inputs)[which(inputs[[1]] == label)]
    cells[cells$r == paste0("B", row), ]
  }
  seed <- value_cell("Seed")
  expect_identical(seed$v, "42")
  expect_true(is.na(seed$c_t) || seed$c_t == "")
  expect_equal(as.numeric(value_cell("Buffer")$v), 0.05)
  expect_identical(value_cell("Mode of sampling")$c_t, "inlineStr")
})

test_that("every type of sampling gives a workbook that opens", {
  for (type in c(
    SAMPLING_TYPE_SIMPLE,
    SAMPLING_TYPE_PPS,
    SAMPLING_TYPE_CLUSTER
  )) {
    for (stratified in c(FALSE, TRUE)) {
      for (mode in c(SAMPLING_MODE_FROM_POPULATION, SAMPLING_MODE_ENTER_SIZE)) {
        d <- draw(settings(
          sampling_type = type,
          stratified = stratified,
          sampling_mode = mode
        ))
        file <- saved(d)
        expect_identical(
          unname(openxlsx2::wb_get_sheet_names(file$wb)),
          unname(OUTPUT_SHEETS),
          info = paste(type, stratified, mode)
        )
        expect_equal(
          nrow(openxlsx2::wb_to_df(file$path, sheet = "sample")),
          nrow(d$out$sample),
          info = paste(type, stratified, mode)
        )
      }
    }
  }
})

test_that("the Guide names the sheets of the file like the code does", {
  expect_identical(
    fill_guide_placeholders("{{sheet:summary}}"),
    "sample_summary"
  )
  expect_error(fill_guide_placeholders("{{sheet:no_such}}"), "No sheet")
  text <- fill_guide_placeholders(paste(
    readLines(file.path(app_root, "guide", "results-file.md"), warn = FALSE),
    collapse = "\n"
  ))
  for (sheet in OUTPUT_SHEETS) {
    expect_match(text, sheet, fixed = TRUE)
  }
  expect_match(text, "1048576 rows", fixed = TRUE)
  expect_no_match(text, "{{", fixed = TRUE)
})
