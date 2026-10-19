# The box of the file input shows the name of the chosen file. After a reload a
# browser can put the name back although the new session has no file (issue
# #52): the box does not ask the browser to remember it, and it is cleared when
# the page connects.

skip_if_not_installed("chromote")
skip_if(is.null(chromote::find_chrome()), "Chrome is not available")

test_that("the box of the file name is not restored by the browser and is cleared on connection", {
  app <- AppDriver$new(
    app_dir = app_root,
    name = "file-input",
    load_timeout = 90000
  )
  withr::defer(app$stop())
  Sys.sleep(1.5)
  app$wait_for_idle()

  box <- "$('#frame_file').closest('.input-group').find('input[type=text]')"
  expect_identical(app$get_js(paste0(box, ".attr('autocomplete')")), "off")

  # a name left in the box, as after a reload, then the page connects again
  app$run_js(paste0(box, ".val('previous_sample.csv');"))
  expect_identical(app$get_js(paste0(box, ".val()")), "previous_sample.csv")
  app$run_js("$(document).trigger('shiny:connected');")
  expect_identical(app$get_js(paste0(box, ".val()")), "")
})
