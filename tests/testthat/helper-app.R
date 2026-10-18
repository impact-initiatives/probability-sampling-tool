# Loads the app the way Shiny does: global.R, then the files of R/.
app_root <- normalizePath(testthat::test_path("..", ".."))
shiny::loadSupport(app_root, renv = globalenv(), globalrenv = globalenv())
