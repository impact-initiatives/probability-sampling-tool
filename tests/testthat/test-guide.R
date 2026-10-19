# The guide (issue #18): it documents every column of the tables, and its links
# open the right section.

guide_file <- function(name) {
  paste(readLines(file.path(app_root, name), warn = FALSE), collapse = "\n")
}

test_that("the results page documents every column the tool adds and every summary column", {
  results <- fill_guide_placeholders(paste(
    guide_file("guide/results-columns.md"),
    guide_file("guide/results-summary.md")
  ))
  for (column in unique(c(FRAME_COLUMN_ORDER, SAMPLE_COLUMN_ORDER))) {
    expect_match(results, column, fixed = TRUE, info = column)
  }
  for (column in SUMMARY_COLUMN_ORDER) {
    expect_match(results, column, fixed = TRUE, info = column)
  }
})

test_that("every {{...}} of the guide texts stands for something that exists", {
  files <- list.files(file.path(app_root, "guide"), "\\.md$", full.names = TRUE)
  expect_gt(length(files), 0)
  for (file in files) {
    text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    expect_no_error(fill_guide_placeholders(text))
    expect_false(grepl("\\{\\{", fill_guide_placeholders(text)), info = file)
  }
  expect_error(fill_guide_placeholders("{{NO_SUCH_CONSTANT}}"), "Unknown")
  expect_error(fill_guide_placeholders("{{label:no_such_column}}"), "No label")
})

test_that("the placeholders give the names and values of the code", {
  expect_identical(
    fill_guide_placeholders("{{SAMPLING_TYPE_CLUSTER}} / {{MAX_UPLOAD_MB}}"),
    paste(SAMPLING_TYPE_CLUSTER, "/", MAX_UPLOAD_MB)
  )
  # every label of the tables can be asked for by the name of its column
  for (key in unname(COLUMN_LABELS)) {
    expect_identical(
      fill_guide_placeholders(paste0("{{label:", key, "}}")),
      names(COLUMN_LABELS)[match(key, COLUMN_LABELS)]
    )
  }
})

test_that("the tables of the checks list every check that blocks", {
  for (group_id in names(CHECKS$groups)) {
    markdown <- guide_checks_markdown(group_id)
    rows <- grep("^\\| \\*", strsplit(markdown, "\n")[[1]], value = TRUE)
    blocks <- Filter(
      function(check) {
        check$group == group_id && !identical(check$kind, "warning")
      },
      CHECKS$checks
    )
    expect_length(rows, length(blocks))
    expect_match(markdown, CHECKS$groups[[group_id]]$when, fixed = TRUE)
  }
  # a check that only warns is told about in the note of its group
  expect_match(
    fill_guide_placeholders(guide_checks_markdown("draw")),
    check_title("exceeds_population"),
    fixed = TRUE
  )
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

  app$click("guide-open_cluster")
  Sys.sleep(1)
  app$wait_for_idle()
  expect_identical(active(), "types")
  open_panel <- app$get_js(
    "Array.from(document.querySelectorAll('#guide-types_accordion .accordion-collapse.show')).map(function(e) { return e.closest('.accordion-item').getAttribute('data-value'); }).join(',')"
  )
  expect_identical(open_panel, "cluster")

  app$click("guide-to_app")
  Sys.sleep(1)
  expect_identical(app$get_value(input = "main_nav"), "Sampling frame")
})
