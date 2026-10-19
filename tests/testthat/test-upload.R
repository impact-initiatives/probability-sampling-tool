# Reading of an uploaded file: what is refused, and with which message.

read_text <- function(text, name = "frame.csv") {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeLines(text, path)
  read_uploaded_csv(name, path)
}

test_that("a usable CSV file is read", {
  result <- read_text(c("a,b", "1,2", "3,4"))
  expect_null(result$error)
  expect_identical(names(result$data), c("a", "b"))
  expect_equal(nrow(result$data), 2)
})

test_that("a file that is not a CSV is refused by its name", {
  result <- read_text(c("a,b", "1,2"), name = "frame.xlsx")
  expect_null(result$data)
  expect_identical(
    result$error,
    check_message("csv_format", name = "frame.xlsx")
  )
})

test_that("a file with two columns of the same name is refused, with the names", {
  result <- read_text(c("a,b,a,b,c", "1,2,3,4,5"))
  expect_null(result$data)
  expect_identical(
    result$error,
    check_message("duplicate_columns", columns = "a, b")
  )
  expect_match(result$error, "a, b", fixed = TRUE)

  # the case reported in #53: one column given twice
  one <- read_text(c("Female_total,x,Female_total", "1,2,3"))
  expect_match(one$error, "Female_total", fixed = TRUE)
  expect_no_match(one$error, "Female_total, Female_total", fixed = TRUE)
})
