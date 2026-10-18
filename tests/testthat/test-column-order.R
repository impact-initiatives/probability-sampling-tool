# The columns of the tables and of the CSV files follow one order whatever the
# sampling type and mode (issue #43): the columns the tool adds first, by order
# of importance, then the columns of the sampling frame.

app_root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(app_root, "global.R"), chdir = TRUE)

modes <- c("Sample size based on population", "Enter sample size")
types <- c(
  "Simple random sampling",
  "PPS sampling with replacement",
  "Two-stage PPS cluster sampling with replacement"
)

sampling_input <- function(topup, samp_type, stratified) {
  list(
    topup = topup,
    target = 300,
    conf_level = 0.95,
    e_marg = 0.05,
    pror = 0.5,
    buf = 0.05,
    samp_type = samp_type,
    stratified = stratified,
    strata = "Region",
    col_psu = "VDC_code",
    colpop = "HH_total",
    cls = 5,
    ICC = 0.06,
    seed_value = 7
  )
}

# in the order of `reference`, ignoring the names of `reference` not in `x`
in_reference_order <- function(x, reference) {
  known <- x[x %in% reference]
  identical(known, reference[reference %in% known])
}

# the frame as the app reads it: fread() gives a data.table
read_frame <- function() {
  data.table::fread(file.path(app_root, "data_example.csv"))
}

test_that("the tables and the CSV files have the same column order in every configuration", {
  for (frame_class in c("data.table", "data.frame")) {
    data <- read_frame()
    if (frame_class == "data.frame") {
      data <- as.data.frame(data)
    }
    user_columns <- names(data)

    for (topup in modes) {
      for (samp_type in types) {
        for (stratified in c("Not stratified", "Stratified")) {
          label <- paste(frame_class, topup, samp_type, stratified, sep = " / ")
          input <- sampling_input(topup, samp_type, stratified)

          frame <- format_sampling_frame(data, input)
          out <- run_with_seed(7, make_sample(data, input))
          sample <- out$sample
          summary <- out$summary_sample

          # the tool's columns first, then the frame's, in their own order
          expect_identical(
            names(frame)[seq_along(frame_column_order)],
            frame_column_order,
            label = paste(label, "frame")
          )
          expect_identical(
            names(frame)[-seq_along(frame_column_order)],
            user_columns,
            label = paste(label, "frame user columns")
          )
          expect_identical(
            names(sample)[seq_along(sample_column_order)],
            sample_column_order,
            label = paste(label, "sample")
          )
          expect_identical(
            names(sample)[-seq_along(sample_column_order)],
            user_columns,
            label = paste(label, "sample user columns")
          )

          # nothing is lost or duplicated, the known columns keep their order
          expect_false(anyDuplicated(names(summary)) > 0, label = label)
          expect_true(
            in_reference_order(names(summary), summary_column_order),
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
