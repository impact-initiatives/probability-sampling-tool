# The example data comes with its columns already chosen; an uploaded file
# does not.

skip_if_not_installed("chromote")
skip_if(is.null(chromote::find_chrome()), "Chrome is not available")

test_that("the example data pre-selects its columns, an upload selects none", {
  app <- AppDriver$new(
    app_dir = app_root,
    name = "example-columns",
    load_timeout = 90000
  )
  withr::defer(app$stop())

  chosen <- function() {
    vapply(
      COLUMN_FIELDS,
      function(field) app$get_value(input = field),
      character(1)
    )
  }

  app$set_inputs(data_source = DATA_SOURCE_TEST)
  app$wait_for_idle()
  expect_identical(chosen(), EXAMPLE_COLUMNS[COLUMN_FIELDS])

  app$set_inputs(data_source = DATA_SOURCE_UPLOAD)
  app$wait_for_idle()
  expect_true(all(chosen() == NO_COLUMN))
})
