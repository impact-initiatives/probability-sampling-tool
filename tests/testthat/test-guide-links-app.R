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
  # the Checks section is in the version 2026.1.0, folded: open both to reach its link
  click_text(".accordion-button", "2026.1.0")
  click_text(".accordion-button", "Checks")
  app$run_js(
    "document.querySelector('a[href=\"#guide:data/checks\"]').click();"
  )
  Sys.sleep(2)
  expect_identical(active_tab(), "Your sampling frame")
  expect_identical(panel_open("What the tool checks"), "true")
})

# On shinyapps.io the page rewrites the href of a link into a full URL, so the
# links must not depend on the href starting with `#guide:` (issue #71).
test_that("the links work when the page rewrites their href", {
  app <- AppDriver$new(
    app_dir = app_root,
    name = "guide-links-rewritten",
    load_timeout = 90000
  )
  withr::defer(app$stop())

  app$run_js(
    "document.querySelectorAll('.guide a[href^=\"#guide:\"]').forEach(function(a) { a.setAttribute('href', location.href.split('#')[0] + a.getAttribute('href')); });"
  )
  app$run_js(
    "Array.from(document.querySelectorAll('a.nav-link')).filter(function(a) { return a.getAttribute('data-value') === 'types'; })[0].click();"
  )
  Sys.sleep(1.5)
  app$run_js(
    "Array.from(document.querySelectorAll('#guide-types_accordion .accordion-button')).filter(function(b) { return b.textContent.indexOf('cluster') >= 0; })[0].click();"
  )
  Sys.sleep(1.5)
  app$run_js(
    "document.querySelector('#guide-types_accordion a[href$=\"#guide:data/checks\"]').click();"
  )
  Sys.sleep(2)
  expect_identical(
    app$get_js(
      "Array.from(document.querySelectorAll('.guide .nav-pills a.nav-link.active')).map(function(a) { return a.textContent.trim(); })[0]"
    ),
    "Your sampling frame"
  )
  expect_identical(
    app$get_js(
      "Array.from(document.querySelectorAll('.accordion-button')).filter(function(b) { return b.textContent.trim() === 'What the tool checks'; }).map(function(b) { return b.getAttribute('aria-expanded'); })[0]"
    ),
    "true"
  )
})
