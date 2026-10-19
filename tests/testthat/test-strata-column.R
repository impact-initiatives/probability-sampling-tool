# The stratification column: chosen, and in the sampling frame (checks
# strata_missing and strata_not_found).

frame <- data.frame(region = c("N", "S"), value = 1:2)
settings <- function(...) {
  base <- list(stratified = TRUE, strata_column = "region")
  normalize_params(utils::modifyList(base, list(...)))
}

test_that("a stratification column that is in the frame is accepted", {
  expect_null(validate_strata_column(frame, settings()))
})

test_that("a stratification column that is not in the frame is reported by its name", {
  message <- validate_strata_column(frame, settings(strata_column = "nope"))
  expect_identical(message, check_message("strata_not_found", column = "nope"))
  expect_match(message, "'nope'", fixed = TRUE)
})

test_that("a stratified sample without a chosen column is reported", {
  expected <- check_message("strata_missing")
  expect_identical(
    validate_strata_column(frame, settings(strata_column = "None")),
    expected
  )
  expect_identical(
    validate_strata_column(frame, settings(strata_column = NULL)),
    expected
  )
  # without a frame there is no column to look for, but the choice is still missing
  expect_identical(
    validate_strata_column(NULL, settings(strata_column = "None")),
    expected
  )
})

test_that("nothing is checked when the sample is not stratified", {
  expect_null(validate_strata_column(
    frame,
    settings(stratified = FALSE, strata_column = "nope")
  ))
  expect_null(validate_strata_column(
    frame,
    settings(stratified = FALSE, strata_column = "None")
  ))
})
