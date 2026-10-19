# "Input strata" comes last in the column mapping, so that "Input population"
# does not move when the sample is stratified or not (issue #43).

skip_if_not_installed("chromote")
skip_if(is.null(chromote::find_chrome()), "Chrome is not available")

test_that("the population field keeps its place whether the sample is stratified or not", {
  app <- AppDriver$new(
    app_dir = normalizePath(testthat::test_path("..", "..")),
    name = "input-order",
    load_timeout = 90000
  )
  withr::defer(app$stop())

  app$run_js(
    "Array.from(document.querySelectorAll('.navbar .nav-link')).find(function(a) { return a.textContent.trim() === 'Sampling frame'; }).click();"
  )
  Sys.sleep(1.5)
  app$wait_for_idle()

  left <- function(id) {
    app$get_js(sprintf(
      "Math.round(document.getElementById('%s').parentElement.querySelector('.selectize-control').getBoundingClientRect().left)",
      id
    ))
  }
  app$set_inputs(
    sampling_type = "Two-stage PPS cluster sampling with replacement"
  )
  app$set_inputs(stratified = FALSE)
  app$wait_for_idle()
  Sys.sleep(0.5)
  population_alone <- left("population_column")

  app$set_inputs(stratified = TRUE)
  app$wait_for_idle()
  Sys.sleep(0.5)
  expect_equal(left("population_column"), population_alone)
  expect_gt(left("strata_column"), left("population_column"))
})
