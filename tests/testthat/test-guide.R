# The guide has to stay true to the app (issue #18): the names it documents are
# the ones the code uses, and its links work.

app_root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(app_root, "global.R"), chdir = TRUE)

guide_file <- function(name) {
  paste(readLines(file.path(app_root, name), warn = FALSE), collapse = "\n")
}

test_that("the results page documents every column the tool adds and every summary column", {
  results <- paste(
    guide_file("guide/results-columns.md"),
    guide_file("guide/results-summary.md")
  )
  for (column in unique(c(frame_column_order, sample_column_order))) {
    expect_match(results, column, fixed = TRUE, info = column)
  }
  for (column in summary_column_order) {
    expect_match(results, column, fixed = TRUE, info = column)
  }
})

test_that("the release notes give the old and the new names of the types and of the columns", {
  news <- guide_file("NEWS.md")
  renamed <- c(
    "Simple random", "Simple random sampling",
    "Simple random - allocation", "PPS sampling with replacement",
    "Cluster sampling", "Two-stage PPS cluster sampling with replacement",
    "Stratification", "Stratum",
    "Clusters drawn", "Distinct PSUs", "Effective sample size", "Buffer"
  )
  for (name in renamed) expect_match(news, name, fixed = TRUE, info = name)
})

test_that("every check cited in the guide exists in the code", {
  guide <- paste(
    vapply(
      c("file", "columns", "parameters", "cluster", "draw"),
      function(x) guide_file(paste0("guide/checks-", x, ".md")),
      ""
    ),
    collapse = "\n"
  )
  code <- paste(guide_file("global.R"), guide_file("server.R"), sep = "\n")
  messages <- c(
    "Please select a stratification variable",
    "has columns with protected names",
    "Please select a cluster (PSU) column.",
    "was not found in the uploaded data",
    "contains duplicate values",
    "Please select a population column for this sampling type.",
    "was not found in the uploaded dataset.",
    "is not a numeric column. Select a numeric population column.",
    "variables must be different columns.",
    "has missing values",
    "has negative values",
    "Enter a target sample size: a whole number greater than 0.",
    "Confidence level must be a number strictly between 0 and 1.",
    "Error margin must be a number strictly between 0 and 1.",
    "Buffer must be a number greater than or equal to 0.",
    "Cluster size must be a whole number of at least 2.",
    "ICC must be a number between 0 and 1.",
    "Seed must be a whole number between -2147483647 and 2147483647.",
    "PSUs smaller than the cluster size",
    "Insufficient stratum capacity",
    "Draw blocked",
    "is not a CSV file. Only .csv files are accepted",
    "could not be read as a CSV table (empty or damaged file)",
    "Please upload a CSV file before applying."
  )
  # the two messages the guide words a little differently from the code
  in_guide <- sub("has columns with protected names", "has column(s) with protected names", messages, fixed = TRUE)
  for (i in seq_along(messages)) {
    expect_true(grepl(in_guide[i], guide, fixed = TRUE), info = paste("guide:", in_guide[i]))
    code_text <- sub("has columns with protected names", "column(s) with protected names", messages[i], fixed = TRUE)
    expect_true(grepl(code_text, code, fixed = TRUE), info = paste("code:", code_text))
  }
})

test_that("the guide gives the upload limit of the app", {
  expect_match(guide_file("guide/data-file.md"), "{{max_upload_mb}}", fixed = TRUE)
})

skip_if_not_installed("chromote")
skip_if(is.null(chromote::find_chrome()), "Chrome is not available")

test_that("the links of the guide open its sections and the app", {
  app <- AppDriver$new(
    app_dir = app_root,
    name = "guide",
    load_timeout = 90000
  )
  withr::defer(app$stop())
  Sys.sleep(1.5)
  app$wait_for_idle()

  active <- function() {
    app$get_js(
      "(function() { var a = document.querySelector('.guide .nav-link.active'); return a ? a.getAttribute('data-value') : null; })()"
    )
  }
  expect_identical(active(), "start")

  app$click("guide_open_cluster")
  Sys.sleep(1)
  app$wait_for_idle()
  expect_identical(active(), "types")
  open_panel <- app$get_js(
    "Array.from(document.querySelectorAll('#types_accordion .accordion-collapse.show')).map(function(e) { return e.closest('.accordion-item').getAttribute('data-value'); }).join(',')"
  )
  expect_identical(open_panel, "cluster")

  app$click("guide_to_app")
  Sys.sleep(1)
  expect_identical(app$get_value(input = "main_nav"), "Sampling frame")
})
