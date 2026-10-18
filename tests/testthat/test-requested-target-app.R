# The warning about a sample size above the population is shown after Apply and
# again after Sample, and the tables use the Requested labels (issue #27).

skip_if_not_installed("chromote")
skip_if(is.null(chromote::find_chrome()), "Chrome is not available")

test_that("the capped sample warning is shown after Apply and after Sample", {
  app <- AppDriver$new(
    app_dir = normalizePath(testthat::test_path("..", "..")),
    name = "requested-target",
    load_timeout = 90000
  )
  withr::defer(app$stop())

  go_tab <- function(name) {
    app$run_js(sprintf(
      "Array.from(document.querySelectorAll('.navbar .nav-link')).find(function(a) { return a.textContent.trim() === '%s'; }).click();",
      name
    ))
    Sys.sleep(1.5)
    app$wait_for_idle()
  }
  modal_text <- function() {
    app$get_js(
      "var m = document.querySelector('.modal.show .alert'); m ? m.innerText : ''"
    )
  }
  close_modal <- function() {
    app$run_js(
      "var b = document.querySelector('.modal.show .modal-footer button'); if (b) b.click();"
    )
    Sys.sleep(1)
  }
  headers <- function(id) {
    app$get_js(sprintf(
      "Array.from(document.querySelectorAll('#%s table thead th')).map(function(h) { return h.innerText.trim(); })",
      id
    ))
  }

  app$set_inputs(
    data_source = "test",
    topup = "Enter sample size",
    samp_type = "Simple random sampling",
    stratified = "Not stratified"
  )
  app$set_inputs(target = 1000)
  go_tab("Sampling frame")
  app$click("f_apply")
  app$wait_for_idle()
  Sys.sleep(1)
  expect_match(modal_text(), "will be capped")
  close_modal()
  expect_true("Requested Target" %in% unlist(headers("target_frame")))
  expect_true(
    "Requested Target (with buffer)" %in% unlist(headers("target_frame"))
  )

  go_tab("Sampling")
  app$click("desButton")
  app$wait_for_idle()
  Sys.sleep(1)
  expect_match(modal_text(), "was capped")
  close_modal()
  expect_true("Requested Target (with buffer)" %in% unlist(headers("test_1")))

  # a target within the population shows no warning, neither at Apply nor Sample
  go_tab("Sampling frame")
  app$set_inputs(target = 100)
  app$click("f_apply")
  app$wait_for_idle()
  Sys.sleep(1)
  expect_identical(modal_text(), "")
  go_tab("Sampling")
  app$click("desButton")
  app$wait_for_idle()
  Sys.sleep(1)
  expect_identical(modal_text(), "")
})
