# What one row of the frame is, said once for each type (ONE_ROW and
# ONE_ROW_DETAIL) and read by the Guide; the README repeats the short forms
# (issue #60).

read_text <- function(...) {
  paste(readLines(file.path(app_root, ...), warn = FALSE), collapse = "\n")
}

test_that("every type has a short and a detailed phrase", {
  expect_identical(names(ONE_ROW), c("simple", "pps", "cluster"))
  expect_identical(names(ONE_ROW_DETAIL), names(ONE_ROW))
  expect_true(all(nzchar(ONE_ROW)) && all(nzchar(ONE_ROW_DETAIL)))
})

test_that("the Guide fills the phrases and an unknown type is an error", {
  expect_identical(fill_guide_placeholders("{{one_row:pps}}"), ONE_ROW[["pps"]])
  expect_identical(
    fill_guide_placeholders("{{one_row_detail:cluster}}"),
    ONE_ROW_DETAIL[["cluster"]]
  )
  expect_error(fill_guide_placeholders("{{one_row:no_such}}"), "No type")
})

test_that("the comparison tables and the type pages use the phrases of the code", {
  types <- c(
    simple = "type-simple-random.md",
    pps = "type-pps.md",
    cluster = "type-cluster.md"
  )
  comparison <- fill_guide_placeholders(read_text("guide", "data-types.md"))
  for (type in names(types)) {
    expect_match(comparison, ONE_ROW[[type]], fixed = TRUE, info = type)
    page <- fill_guide_placeholders(read_text("guide", types[[type]]))
    expect_match(page, ONE_ROW_DETAIL[[type]], fixed = TRUE, info = type)
  }
})

test_that("the cards of the Start page and the README say the same", {
  start <- as.character(htmltools::renderTags(guide_start(identity))$html)
  readme <- read_text("README.md")
  for (type in names(ONE_ROW)) {
    expect_match(
      gsub("&amp;", "&", start, fixed = TRUE),
      ONE_ROW[[type]],
      fixed = TRUE,
      info = type
    )
    expect_match(readme, ONE_ROW[[type]], fixed = TRUE, info = type)
  }
})
