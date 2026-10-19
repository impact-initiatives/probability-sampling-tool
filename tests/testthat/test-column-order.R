# The columns of the tables and of the CSV files follow one order whatever the
# sampling type and mode (issue #43): the columns the tool adds first, by order
# of importance, then the columns of the sampling frame.

modes <- c("Sample size based on population", "Enter sample size")
types <- c(
  "Simple random sampling",
  "PPS sampling with replacement",
  "Two-stage PPS cluster sampling with replacement"
)

sampling_input <- function(sampling_mode, sampling_type, stratified) {
  normalize_params(list(
    sampling_mode = sampling_mode,
    target_size = 300,
    confidence_level = 0.95,
    error_margin = 0.05,
    proportion = 0.5,
    buffer = 0.05,
    sampling_type = sampling_type,
    stratified = stratified,
    strata_column = "strata",
    psu_column = "psu_id",
    population_column = "population",
    cluster_size = 5,
    icc = 0.06,
    seed = 7
  ))
}

# in the order of `reference`, ignoring the names of `reference` not in `x`
in_reference_order <- function(x, reference) {
  known <- x[x %in% reference]
  identical(known, reference[reference %in% known])
}

# the frame as the app reads it: fread() gives a data.table
read_frame <- function() {
  data.table::fread(file.path(app_root, "data", "data_example.csv"))
}

test_that("the tables and the CSV files have the same column order in every configuration", {
  for (frame_class in c("data.table", "data.frame")) {
    data <- read_frame()
    if (frame_class == "data.frame") {
      data <- as.data.frame(data)
    }
    user_columns <- names(data)

    for (sampling_mode in modes) {
      for (sampling_type in types) {
        for (stratified in c(FALSE, TRUE)) {
          label <- paste(
            frame_class,
            sampling_mode,
            sampling_type,
            stratified,
            sep = " / "
          )
          input <- sampling_input(sampling_mode, sampling_type, stratified)

          frame <- format_sampling_frame(data, input)
          out <- run_with_seed(7, make_sample(data, input))
          sample <- out$sample
          summary <- out$summary_sample

          # the tool's columns first, then the frame's, in their own order
          expect_identical(
            names(frame)[seq_along(FRAME_COLUMN_ORDER)],
            FRAME_COLUMN_ORDER,
            label = paste(label, "frame")
          )
          expect_identical(
            names(frame)[-seq_along(FRAME_COLUMN_ORDER)],
            user_columns,
            label = paste(label, "frame user columns")
          )
          expect_identical(
            names(sample)[seq_along(SAMPLE_COLUMN_ORDER)],
            SAMPLE_COLUMN_ORDER,
            label = paste(label, "sample")
          )
          expect_identical(
            names(sample)[-seq_along(SAMPLE_COLUMN_ORDER)],
            user_columns,
            label = paste(label, "sample user columns")
          )

          # nothing is lost or duplicated, the known columns keep their order
          expect_false(anyDuplicated(names(summary)) > 0, label = label)
          expect_true(
            in_reference_order(names(summary), SUMMARY_COLUMN_ORDER),
            label = paste(label, "summary order")
          )
          expect_identical(
            tail(names(summary), 2),
            c("Seed", "Sampling type"),
            label = paste(label, "summary end")
          )
        }
      }
    }
  }
})

test_that("order_columns keeps the columns it does not know and ignores the missing ones", {
  data <- data.frame(b = 1, a = 2, z = 3, c = 4)
  expect_named(
    order_columns(data, c("c", "missing", "a")),
    c("c", "a", "b", "z")
  )
  expect_named(order_columns(data, character()), names(data))
  expect_identical(nrow(order_columns(data, "z")), 1L)
})
