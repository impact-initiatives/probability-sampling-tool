# The guide (issue #18): it documents every column of the tables, and its links
# open the right section.

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
