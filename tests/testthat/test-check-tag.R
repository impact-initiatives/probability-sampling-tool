# The release tag is the version of the app: the script of the CI that checks it
# (issue #56), tried on the real constants file and on small ones.

script <- file.path(app_root, ".github", "scripts", "check-tag.sh")

run_check <- function(tag, constants = NULL) {
  args <- c(
    shQuote(script),
    shQuote(tag),
    if (!is.null(constants)) shQuote(constants)
  )
  out <- suppressWarnings(system2(
    "bash",
    c("-c", shQuote(paste(args, collapse = " "))),
    stdout = TRUE,
    stderr = TRUE
  ))
  list(status = attr(out, "status") %||% 0L, text = paste(out, collapse = "\n"))
}

skip_if(Sys.which("bash") == "", "bash is not available")
skip_on_os("windows")

test_that("the tag of the version of the app is accepted", {
  result <- run_check(
    paste0("v", APP_VERSION),
    file.path(app_root, "R", "constants.R")
  )
  expect_identical(result$status, 0L)
})

test_that("another tag, or a tag that is not a version, is refused with a message", {
  file <- withr::local_tempfile(fileext = ".R")
  writeLines('APP_VERSION <- "2026.1.0"', file)
  expect_identical(run_check("v2026.1.0", file)$status, 0L)

  other <- run_check("v2026.1.1", file)
  expect_gt(other$status, 0L)
  expect_match(other$text, "APP_VERSION is '2026.1.0'", fixed = TRUE)

  for (tag in c("2026.1.0", "v2026.1.0-rc1", "release")) {
    result <- run_check(tag, file)
    expect_gt(result$status, 0L)
    expect_match(result$text, "not a version tag", fixed = TRUE)
  }
})

test_that("a constants file without APP_VERSION is refused", {
  file <- withr::local_tempfile(fileext = ".R")
  writeLines("X <- 1", file)
  result <- run_check("v2026.1.0", file)
  expect_gt(result$status, 0L)
  expect_match(result$text, "APP_VERSION was not found", fixed = TRUE)
})
