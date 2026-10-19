# Golden master (issue #36): every scenario of helper-golden-master.R gives the
# same results as when the reference was recorded, before the restructuring.
#
# To record the reference again after a change of behaviour that is wanted:
#   UPDATE_GOLDEN=true Rscript -e 'testthat::test_file("tests/testthat/test-golden-master.R")'
#
# It is skipped on a CI server: the reference was recorded on one machine, and it
# holds fingerprints of the tables, which contain the paths of the package
# library, and of the samples, whose weighted draws can differ between platforms.

reference_file <- file.path(
  app_root,
  "tests",
  "testthat",
  "fixtures",
  "golden-master.rds"
)

test_that("the app gives the same results as the recorded reference", {
  skip_on_ci()
  withr::local_dir(app_root)
  server <- source("server.R")$value
  data_path <- file.path(app_root, "data", "data_example.csv")

  scenarios <- golden_scenarios()
  records <- lapply(
    scenarios,
    run_golden_scenario,
    server = server,
    data_path = data_path
  )

  if (identical(Sys.getenv("UPDATE_GOLDEN"), "true")) {
    dir.create(dirname(reference_file), showWarnings = FALSE)
    saveRDS(records, reference_file, version = 2)
    skip("reference recorded")
  }

  reference <- readRDS(reference_file)
  expect_setequal(names(records), names(reference))
  for (id in names(records)) {
    expect_equal(records[[id]], reference[[id]], info = id)
  }
})
