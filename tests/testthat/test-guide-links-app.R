# A link of a text of the Guide, written `[text](#guide:tab/panel)`, goes to its tab
# and opens its panel (issue #63).

skip_if_not_installed("chromote")
skip_if(is.null(chromote::find_chrome()), "Chrome is not available")

test_that("a link in What's new goes to What the tool checks and opens it", {
  app <- AppDriver$new(
    app_dir = app_root,
    name = "guide-links",
    load_timeout = 90000
  )
  withr::defer(app$stop())

  click_text <- function(selector, text) {
    app$run_js(sprintf(
      "Array.from(document.querySelectorAll('%s')).filter(function(b) { return b.textContent.trim().indexOf('%s') === 0; })[0].click();",
      selector,
      text
    ))
    Sys.sleep(1.5)
  }
  active_tab <- function() {
    app$get_js(
      "Array.from(document.querySelectorAll('.guide .nav-pills a.nav-link.active')).map(function(a) { return a.textContent.trim(); })[0]"
    )
  }
  panel_open <- function(title) {
    app$get_js(sprintf(
      "Array.from(document.querySelectorAll('.accordion-button')).filter(function(b) { return b.textContent.trim() === '%s'; }).map(function(b) { return b.getAttribute('aria-expanded'); })[0]",
      title
    ))
  }

  click_text("a.nav-link", "What")
  expect_identical(active_tab(), "What's new")
  # the Checks section is folded: open it to reach its link
  click_text(".accordion-button", "Checks")
  app$run_js(
    "document.querySelector('a[href=\"#guide:data/checks\"]').click();"
  )
  Sys.sleep(2)
  expect_identical(active_tab(), "Your sampling frame")
  expect_identical(panel_open("What the tool checks"), "true")
})
