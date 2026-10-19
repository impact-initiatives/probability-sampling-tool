# The version of the app, and the commit from BUILD_INFO when the deployment
# wrote one (issue #60).

write_build_info <- function(text) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(text, file.path(root, "BUILD_INFO"))
  root
}

test_that("the version of the app is a calendar version", {
  expect_match(APP_VERSION, "^[0-9]{4}\\.[0-9]+\\.[0-9]+$")
  expect_identical(app_info(withr::local_tempdir())$version, APP_VERSION)
})

test_that("the commit is added when BUILD_INFO has one", {
  info <- app_info(write_build_info("Commit: abc1234"))
  expect_identical(info$commit, "abc1234")
  expect_identical(info$label, paste0(APP_VERSION, " (abc1234)"))
})

test_that("without BUILD_INFO only the version is shown", {
  info <- app_info(withr::local_tempdir())
  expect_true(is.na(info$commit))
  expect_identical(info$label, APP_VERSION)
})

test_that("the app never fails with an empty or unreadable BUILD_INFO", {
  for (text in list("", "not a dcf file at all", "Commit:")) {
    expect_no_error(info <- app_info(write_build_info(text)))
    expect_identical(info$label, APP_VERSION)
  }
})
