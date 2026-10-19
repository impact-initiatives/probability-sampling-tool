# The buffer is a share of the target between 0 and MAX_BUFFER (check buffer).

settings <- function(...) {
  base <- list(
    sampling_mode = SAMPLING_MODE_FROM_POPULATION,
    sampling_type = SAMPLING_TYPE_SIMPLE,
    confidence_level = 0.95,
    error_margin = 0.05,
    buffer = 0.05
  )
  normalize_params(utils::modifyList(base, list(...)))
}

test_that("a buffer from 0 to the maximum is accepted", {
  for (buffer in c(0, 0.05, 1, MAX_BUFFER)) {
    expect_null(validate_parameters(settings(buffer = buffer)), info = buffer)
  }
})

test_that("a buffer below 0, above the maximum or not a number is reported with the maximum", {
  expected <- check_message("buffer", max = MAX_BUFFER)
  expect_match(expected, paste("between 0 and", MAX_BUFFER), fixed = TRUE)
  for (buffer in list(-0.1, MAX_BUFFER + 0.01, 50, NA, "a")) {
    expect_identical(
      validate_parameters(settings(buffer = buffer)),
      expected,
      info = format(buffer)
    )
  }
})

test_that("the buffer is not checked when the size is entered", {
  expect_null(validate_parameters(settings(
    sampling_mode = SAMPLING_MODE_ENTER_SIZE,
    target_size = 100,
    buffer = 50
  )))
})
