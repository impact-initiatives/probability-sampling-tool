# The cluster size and the design effect of the summary are set by the design:
# a PSU drawn several times is several independent clusters (issue #21).

test_that("the cluster summary has one cluster size, one DEFF and one effective sample size", {
  frame <- data.frame(
    region = rep(c("N", "S"), each = 30),
    psu_name = sprintf("P%02d", 1:60),
    population = rep(c(40, 60, 80, 120, 200, 400), 10)
  )
  input <- normalize_params(list(
    sampling_mode = "Sample size based on population",
    target_size = NA,
    confidence_level = 0.95,
    error_margin = 0.05,
    proportion = 0.5,
    buffer = 0.05,
    sampling_type = "Two-stage PPS cluster sampling with replacement",
    stratified = TRUE,
    strata_column = "region",
    psu_column = "psu_name",
    population_column = "population",
    cluster_size = 5,
    icc = 0.1,
    seed = 1
  ))
  summary <- run_with_seed(1, make_sample(frame, input))$summary_sample

  expect_true(all(
    c("Cluster size", "DEFF", "Effective sample size") %in%
      names(summary)
  ))
  expect_false(any(grepl("realized|planned", names(summary))))

  expect_equal(summary$`Cluster size`, rep(5, 2))
  expect_equal(summary$DEFF, rep(1 + (5 - 1) * 0.1, 2))
  expect_equal(
    summary$`Effective sample size`,
    round(summary$`Surveys` / summary$DEFF)
  )
  # some PSUs are drawn several times: the design effect does not depend on it
  expect_true(all(summary$`Clusters drawn` > summary$`Distinct PSUs`))
})

test_that("with Enter sample size the cluster summary still shows the design effect and the effective sample size", {
  frame <- data.frame(
    region = rep(c("N", "S"), each = 30),
    psu_name = sprintf("P%02d", 1:60),
    population = rep(c(40, 60, 80, 120, 200, 400), 10)
  )
  settings <- function(icc, type = SAMPLING_TYPE_CLUSTER) {
    normalize_params(list(
      sampling_mode = SAMPLING_MODE_ENTER_SIZE,
      target_size = 300,
      confidence_level = 0.95,
      error_margin = 0.05,
      proportion = 0.5,
      buffer = 0.05,
      sampling_type = type,
      stratified = "Not stratified",
      strata_column = "None",
      psu_column = "psu_name",
      population_column = "population",
      cluster_size = 5,
      icc = icc,
      seed = 1
    ))
  }
  low <- run_with_seed(1, make_sample(frame, settings(0.06)))
  high <- run_with_seed(1, make_sample(frame, settings(0.5)))

  # the design effect and what the surveys are worth, from the ICC entered
  expect_equal(low$summary_sample$DEFF, 1 + (5 - 1) * 0.06)
  expect_equal(high$summary_sample$DEFF, 1 + (5 - 1) * 0.5)
  expect_equal(low$summary_sample$ICC, 0.06)
  expect_equal(
    low$summary_sample$`Effective sample size`,
    round(low$summary_sample$Surveys / low$summary_sample$DEFF)
  )
  expect_equal(low$summary_sample$`Effective sample size`, 242)

  # the ICC does not change the size or the draw: the number entered stays
  expect_equal(low$summary_sample$Surveys, 300)
  expect_equal(low$summary_sample$Surveys, high$summary_sample$Surveys)
  expect_identical(low$sample, high$sample)

  # the parameters of the computation are not used, so not shown
  expect_false(any(
    c("Buffer", "Confidence level", "Error margin") %in%
      names(low$summary_sample)
  ))

  # the other types have no design effect
  simple <- run_with_seed(
    1,
    make_sample(frame, settings(0.06, SAMPLING_TYPE_SIMPLE))
  )
  expect_false(any(
    c("ICC", "DEFF", "Effective sample size") %in% names(simple$summary_sample)
  ))
})
