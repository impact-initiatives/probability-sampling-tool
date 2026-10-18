# The cluster size and the design effect of the summary are set by the design:
# a PSU drawn several times is several independent clusters (issue #21).

test_that("the cluster summary has one cluster size, one DEFF and one effective sample size", {
  frame <- data.frame(
    region = rep(c("N", "S"), each = 30),
    psu_name = sprintf("P%02d", 1:60),
    population = rep(c(40, 60, 80, 120, 200, 400), 10)
  )
  input <- list(
    topup = "Sample size based on population",
    target = NA,
    conf_level = 0.95,
    e_marg = 0.05,
    pror = 0.5,
    buf = 0.05,
    samp_type = "Two-stage PPS cluster sampling with replacement",
    stratified = "Stratified",
    strata = "region",
    col_psu = "psu_name",
    colpop = "population",
    cls = 5,
    ICC = 0.1,
    seed_value = 1
  )
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
