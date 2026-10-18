# The four result tables share one look (issue #43): a filter per column,
# numbers under the text of their header, shorter numbers for the columns the
# tool adds.

skip_if_not_installed("chromote")
skip_if(is.null(chromote::find_chrome()), "Chrome is not available")

test_that("the result tables have filters and aligned short numbers", {
  app <- AppDriver$new(
    app_dir = normalizePath(testthat::test_path("..", "..")),
    name = "result-tables",
    load_timeout = 90000,
    width = 1500,
    height = 950
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

  app$set_inputs(
    data_source = "test",
    sampling_type = "PPS sampling with replacement",
    stratified = "Not stratified"
  )
  app$wait_for_idle()
  app$set_inputs(population_column = "HH_total")
  app$wait_for_idle()
  app$click("apply_button")
  app$wait_for_idle()
  Sys.sleep(1)
  go_tab("Sampling")
  app$click("draw_button")
  app$wait_for_idle()
  Sys.sleep(2)

  table_info <- function(id) {
    app$get_js(sprintf(
      "(function() {
        var r = document.getElementById('%s');
        return {
          filters: r.querySelectorAll('.dataTables_scrollHead input').length,
          columns: r.querySelectorAll('.dataTables_scrollHead thead tr:first-child th').length
        };
      })()",
      id
    ))
  }
  for (id in c("summary_table", "sample_table")) {
    info <- table_info(id)
    expect_equal(info$filters, info$columns, label = paste(id, "filters"))
  }

  # the numbers end under the text of their header
  edge <- function(selector) {
    app$get_js(sprintf(
      "(function() {
        var e = document.querySelector('%s'); var rg = document.createRange();
        rg.selectNodeContents(e); return Math.round(rg.getBoundingClientRect().right);
      })()",
      selector
    ))
  }
  head_right <- edge(
    "#sample_table .dataTables_scrollHead thead tr:first-child th:nth-child(2)"
  )
  value_right <- edge(
    "#sample_table .dataTables_scrollBody tbody tr:first-child td:nth-child(2)"
  )
  expect_lte(abs(head_right - value_right), 2)

  # PST_prob is shown with 4 significant digits at most
  prob <- app$get_js(
    "(function() {
      var heads = Array.from(document.querySelectorAll('#sample_table .dataTables_scrollHead thead tr:first-child th'));
      var i = heads.findIndex(function(h) { return h.innerText.trim() === 'PST_prob'; });
      return document.querySelectorAll('#sample_table .dataTables_scrollBody tbody tr:first-child td')[i].innerText.trim();
    })()"
  )
  expect_lte(nchar(gsub("^0\\.0*|\\.", "", prob)), 4)
})
