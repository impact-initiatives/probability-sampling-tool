# The labels of the fields of the form are defined once (FIELD_LABELS): the
# form, the Guide and the summary read them there (issue #60).

test_that("every field of the settings has a distinct label", {
  expect_true(all(PARAM_FIELDS %in% names(FIELD_LABELS)))
  expect_false(anyDuplicated(FIELD_LABELS) > 0)
  expect_true(all(nzchar(FIELD_LABELS)))
})

test_that("the form shows the label of each of its fields", {
  html <- paste(
    as.character(htmltools::renderTags(frame_tab())$html),
    collapse = ""
  )
  for (field in names(FIELD_LABELS)) {
    expect_match(html, FIELD_LABELS[[field]], fixed = TRUE, info = field)
  }
})

test_that("the summary labels its parameters like the form", {
  for (field in c(
    "cluster_size",
    "icc",
    "buffer",
    "confidence_level",
    "error_margin",
    "seed"
  )) {
    expect_identical(column_label(field), unname(FIELD_LABELS[[field]]))
    expect_true(FIELD_LABELS[[field]] %in% SUMMARY_COLUMN_ORDER)
  }
  expect_setequal(SUMMARY_COLUMN_ORDER, names(SUMMARY_LABELS))
})

test_that("the Guide fills the label of a field, and an unknown field is an error", {
  expect_identical(
    fill_guide_placeholders("{{field:error_margin}}"),
    "Error margin"
  )
  expect_error(fill_guide_placeholders("{{field:no_such}}"), "No label")
})

test_that("the messages of the checks name a field like the form does", {
  expect_match(check_message("seed"), "^Seed must be")
  expect_match(check_message("error_margin"), "^Error margin must be")
  expect_match(
    check_message("strata_missing"),
    "uncheck 'Stratified'",
    fixed = TRUE
  )
  expect_match(
    check_message("psu_not_found", column = "x"),
    "^Input cluster 'x'"
  )
  expect_no_match(check_message("seed"), "{{", fixed = TRUE)
})

test_that("the Guide shows the labels of the fields in the checks tables", {
  table <- fill_guide_placeholders(guide_checks_markdown("parameters"))
  expect_match(table, "Confidence level must be", fixed = TRUE)
  expect_match(table, "Buffer must be a number between 0 and 2", fixed = TRUE)
  expect_no_match(table, "{{", fixed = TRUE)
})
