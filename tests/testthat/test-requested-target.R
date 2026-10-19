# What is requested is labelled as requested (issue #27): the draw is capped to
# the population of the stratum, so the target can be larger than what is drawn.

frame_24 <- function() {
  data.frame(
    region = rep(c("N", "S", "E", "W"), each = 6),
    psu_name = sprintf("P%02d", 1:24),
    population = rep(c(50, 60, 70, 80, 90, 100), 4)
  )
}

sampling_input <- function(
  sampling_type,
  stratified,
  sampling_mode,
  target_size = 100
) {
  normalize_params(list(
    sampling_mode = sampling_mode,
    target_size = target_size,
    confidence_level = 0.95,
    error_margin = 0.05,
    proportion = 0.5,
    buffer = 0.05,
    sampling_type = sampling_type,
    stratified = stratified,
    strata_column = "region",
    psu_column = "psu_name",
    population_column = "population",
    cluster_size = 5,
    icc = 0.06,
    seed = 1
  ))
}

test_that("the target table and the sample summary say requested", {
  modes <- c("Enter sample size", "Sample size based on population")
  types <- c(
    "Simple random sampling",
    "PPS sampling with replacement",
    "Two-stage PPS cluster sampling with replacement"
  )
  for (sampling_mode in modes) {
    for (sampling_type in types) {
      for (stratified in c(FALSE, TRUE)) {
        input <- sampling_input(sampling_type, stratified, sampling_mode)
        label <- paste(sampling_mode, sampling_type, stratified, sep = " / ")

        targets <- create_targets(
          format_sampling_frame(frame_24(), input),
          input
        )
        shown <- names(label_target_table(targets))
        expect_true(
          all(
            c("Requested Target", "Requested Target (with buffer)") %in% shown
          ),
          label = label
        )
        expect_false(
          any(c("target", "target_with_buffer") %in% shown),
          label = label
        )

        summary <- run_with_seed(
          1,
          make_sample(frame_24(), input)
        )$summary_sample
        expect_true(
          "Requested Target (with buffer)" %in% names(summary),
          label = label
        )
        expect_false("Target (with buffer)" %in% names(summary), label = label)
      }
    }
  }
})

test_that("the labels are display only: the internal target names are kept", {
  input <- sampling_input(
    "Simple random sampling",
    TRUE,
    "Enter sample size"
  )
  targets <- create_targets(format_sampling_frame(frame_24(), input), input)
  expect_true(all(c("target", "target_with_buffer") %in% names(targets)))
  expect_identical(
    label_target_table(targets)$`Requested Target (with buffer)`,
    targets$target_with_buffer
  )
})

test_that("a capped draw shows the requested target next to the drawn surveys", {
  input <- sampling_input(
    "Simple random sampling",
    TRUE,
    "Enter sample size",
    500
  )
  summary <- run_with_seed(1, make_sample(frame_24(), input))$summary_sample
  expect_equal(summary$`Requested Target (with buffer)`, rep(500, 4))
  # Simple random sampling: the population of a stratum is its number of rows (6)
  expect_equal(summary$`Surveys`, rep(6, 4))
})

test_that("the warning modal says will be before the draw and was after it", {
  html <- function(modal) as.character(modal)
  expect_match(html(exceeds_population_modal("N, S")), "will be capped")
  expect_match(
    html(exceeds_population_modal("N, S", drawn = TRUE)),
    "was capped"
  )
  expect_match(html(exceeds_population_modal("N, S")), "N, S", fixed = TRUE)
})

test_that("a target above the population warns in the random types and blocks cluster", {
  # frame_24(): the population column sums to 450 per stratum, 1800 in total
  exceeds <- function(sampling_type, stratified) {
    input <- sampling_input(
      sampling_type,
      stratified,
      "Enter sample size",
      5000
    )
    frame <- format_sampling_frame(frame_24(), input)
    targets <- create_targets(frame, input)
    list(
      warning = check_target_vs_population(targets),
      guard = cluster_size_issues(frame, targets, input)
    )
  }
  for (stratified in c(FALSE, TRUE)) {
    for (sampling_type in c(
      "Simple random sampling",
      "PPS sampling with replacement"
    )) {
      r <- exceeds(sampling_type, stratified)
      expect_false(is.null(r$warning), label = paste(sampling_type, stratified))
      expect_null(r$guard)
    }
    # cluster sampling never caps the draw: the target is refused at Apply
    r <- exceeds("Two-stage PPS cluster sampling with replacement", stratified)
    expect_false(
      is.null(r$guard),
      label = paste(
        "Two-stage PPS cluster sampling with replacement",
        stratified
      )
    )
  }
})
