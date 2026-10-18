# The stratification column must be in the sampling frame (check strata_not_found).

frame <- data.frame(region = c("N", "S"), value = 1:2)
settings <- function(...) {
  base <- list(stratified = "Stratified", strata_column = "region")
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

test_that("the column is not looked for when the sample is not stratified or none is chosen", {
  expect_null(validate_strata_column(
    frame,
    settings(stratified = "Not stratified", strata_column = "nope")
  ))
  expect_null(validate_strata_column(frame, settings(strata_column = "None")))
  expect_null(validate_strata_column(frame, settings(strata_column = NULL)))
})
