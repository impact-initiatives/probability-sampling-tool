# Same scenario as test-sample-lifecycle-server.R, in the real page: tables and
# download links are read from the DOM, so what the user can see is what is tested.

skip_if_not_installed("chromote")
skip_if(is.null(chromote::find_chrome()), "Chrome is not available")

test_that("a change of the setup or a new Apply empties the Sampling tab until Sample is clicked again", {
  app <- AppDriver$new(
    app_dir = normalizePath(testthat::test_path("..", "..")),
    name = "sample-lifecycle",
    load_timeout = 90000
  )
  withr::defer(app$stop())

  # Shiny suspends the outputs of a hidden tab: show the tab before reading it
  go_tab <- function(name) {
    app$run_js(sprintf(
      "Array.from(document.querySelectorAll('.navbar .nav-link')).find(function(a) { return a.textContent.trim() === '%s'; }).click();",
      name
    ))
    Sys.sleep(1.5)
    app$wait_for_idle()
  }
  # rows the user can see: a failed req() hides the output with CSS
  has_rows <- function(id) {
    app$get_js(sprintf(
      "Array.from(document.querySelectorAll('#%s table tbody tr')).filter(function(r) { return r.offsetParent !== null && getComputedStyle(r).visibility !== 'hidden' && !r.querySelector('td.dataTables_empty'); }).length > 0",
      id
    ))
  }
  has_link <- function(id) {
    app$get_js(sprintf(
      "var a = document.querySelector('a#%s'); a !== null && !a.classList.contains('disabled')",
      id
    ))
  }
  sample_enabled <- function() {
    app$get_js(
      "var b = document.getElementById('draw_button'); !b.disabled && !b.classList.contains('disabled')"
    )
  }
  draw <- function() {
    app$click("draw_button")
    app$wait_for_idle()
    Sys.sleep(1)
  }

  app$set_inputs(data_source = "test")
  app$click("apply_button")
  app$wait_for_idle()
  go_tab("Sampling")
  expect_true(sample_enabled())
  expect_false(has_rows("sample_table"))
  expect_false(has_link("download_output"))

  draw()
  expect_true(has_rows("sample_table"))
  expect_true(has_rows("summary_table"))
  expect_true(has_link("download_output"))

  # the download is the Excel file of this draw
  file <- app$get_download("download_output")
  expect_true(file.exists(file))
  expect_match(
    basename(file),
    "^PST_Sampling_Output_[0-9]{8}-[0-9]{6}\\.xlsx$"
  )
  expect_identical(
    unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(file))),
    c("inputs", "sample_summary", "sample")
  )

  # a change of the setup hides the sample and the downloads, and says why
  go_tab("Sampling frame")
  app$set_inputs(buffer = 0.1)
  app$wait_for_idle()
  go_tab("Sampling")
  expect_false(has_rows("sample_table"))
  expect_false(has_link("download_output"))
  expect_false(sample_enabled())
  expect_match(
    app$get_js("document.getElementById('sample_status').innerText"),
    "setup changed"
  )

  go_tab("Sampling frame")
  app$click("apply_button")
  app$wait_for_idle()
  go_tab("Sampling")
  expect_false(has_rows("sample_table"))
  expect_false(has_rows("summary_table"))
  expect_false(has_link("download_output"))
  expect_true(sample_enabled())

  draw()
  expect_true(has_rows("sample_table"))
  expect_true(has_link("download_output"))
})
