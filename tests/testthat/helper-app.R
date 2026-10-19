# Loads the app the way Shiny does: global.R, then the files of R/, from the
# folder of the app (R/ reads guide/checks.yml when it is loaded).
app_root <- normalizePath(testthat::test_path("..", ".."))
withr::with_dir(
  app_root,
  shiny::loadSupport(app_root, renv = globalenv(), globalrenv = globalenv())
)
