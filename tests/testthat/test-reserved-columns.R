# The columns the tool adds are prefixed with PST_ (issue #32): a sampling frame
# can use any other name, and a frame that already has a PST_ column is blocked.

# the columns the tool adds to a sample
tool_columns <- c(
  "PST_id_sample",
  "PST_survey",
  "PST_stratum_id",
  "PST_psu_id",
  "PST_pop_size",
  "PST_pop_size_stratum",
  "PST_prob"
)

sampling_types <- c(
  "Simple random sampling",
  "PPS sampling with replacement",
  "Two-stage PPS cluster sampling with replacement"
)

frame_24 <- function() {
  data.frame(
    region = rep(c("N", "S", "E", "W"), each = 6),
    psu_name = sprintf("P%02d", 1:24),
    population = rep(c(50, 60, 70, 80, 90, 100), 4)
  )
}

sampling_input <- function(samp_type, stratified) {
  list(
    topup = "Enter sample size",
    target = 20,
    conf_level = 0.95,
    e_marg = 0.05,
    pror = 0.5,
    buf = 0.05,
    samp_type = samp_type,
    stratified = stratified,
    strata = "region",
    col_psu = "psu_name",
    colpop = "population",
    cls = 5,
    ICC = 0.06,
    seed_value = 1
  )
}

draw <- function(frame, samp_type, stratified) {
  run_with_seed(1, make_sample(frame, sampling_input(samp_type, stratified)))
}

test_that("a frame column named like an old internal column is kept as it is", {
  old_names <- c(
    "id_sampl",
    "strata_id",
    "psu_id",
    "pop_numbers",
    "SumDist",
    "proba",
    "Survey",
    "output"
  )
  for (name in old_names) {
    frame <- frame_24()
    frame[[name]] <- paste0("USER_", name, "_", seq_len(nrow(frame)))
    for (samp_type in sampling_types) {
      for (stratified in c("Not stratified", "Stratified")) {
        label <- paste(name, samp_type, stratified, sep = " / ")
        out <- tryCatch(
          draw(frame, samp_type, stratified),
          error = function(e) {
            fail(paste(label, "->", conditionMessage(e)))
            NULL
          }
        )
        if (is.null(out)) {
          next
        }
        sample <- out$sample
        expect_true(name %in% names(sample), label = label)
        expect_identical(
          sample[[name]],
          frame[[name]][match(sample$psu_name, frame$psu_name)],
          label = label
        )
      }
    }
  }
})

test_that("the frame attached to issue #32 can be sampled", {
  frame <- data.frame(
    region = c(rep("North", 4), rep("South", 12)),
    psu_name = paste0("PSU_", LETTERS[1:16]),
    pop_numbers = c(
      40,
      30,
      20,
      10,
      120,
      95,
      80,
      75,
      60,
      55,
      50,
      45,
      40,
      35,
      30,
      25
    ),
    population = c(
      40,
      30,
      20,
      10,
      120,
      95,
      80,
      75,
      60,
      55,
      50,
      45,
      40,
      35,
      30,
      25
    ),
    Survey = c(NA, 2, 0, 3, 2, 1, 1, 1, 1, 1, 1, 1, 2, 4, 13, 6),
    psu_id = paste0("PSU_", LETTERS[1:16]),
    id_sampl = c("A", "B", rep(NA, 14)),
    SumDist = c("AA", "AA", rep("BB", 14))
  )
  for (samp_type in c(
    "Two-stage PPS cluster sampling with replacement",
    "PPS sampling with replacement"
  )) {
    input <- sampling_input(samp_type, "Stratified")
    input$target <- 30
    input$cls <- 2
    expect_no_error(run_with_seed(1, make_sample(frame, input)))
  }
})

test_that("the sample carries the columns the tool adds, all with the PST_ prefix", {
  out <- draw(
    frame_24(),
    "Two-stage PPS cluster sampling with replacement",
    "Stratified"
  )
  added <- setdiff(names(out$sample), names(frame_24()))
  expect_setequal(added, tool_columns)
  expect_true(all(startsWith(tool_columns, reserved_prefix)))
})

test_that("a frame with a PST_ column is blocked, naming the columns", {
  expect_null(validate_reserved_columns(frame_24()))
  for (name in c(tool_columns, "PST_notes")) {
    frame <- frame_24()
    frame[[name]] <- 1
    expect_match(validate_reserved_columns(frame), name, fixed = TRUE)
  }
  frame <- frame_24()
  frame$PST_prob <- 1
  frame$PST_survey <- 1
  msg <- validate_reserved_columns(frame)
  expect_match(msg, "PST_prob", fixed = TRUE)
  expect_match(msg, "PST_survey", fixed = TRUE)
  expect_no_match(msg, "PST_psu_id", fixed = TRUE)

  # the prefix is case sensitive: this name cannot collide with the tool's
  frame <- frame_24()
  frame$pst_prob <- 1
  expect_null(validate_reserved_columns(frame))
})

test_that("a reloaded sample is blocked at Apply with the reserved columns listed", {
  withr::local_dir(app_root)
  server <- source("server.R")$value

  previous <- draw(
    frame_24(),
    "Simple random sampling",
    "Not stratified"
  )$sample
  path <- withr::local_tempfile(fileext = ".csv")
  write.csv(previous, path, row.names = FALSE)

  shiny::testServer(server, {
    session$setInputs(
      data_source = "upload",
      popdata = list(name = "previous_sample.csv", datapath = path),
      topup = "Enter sample size",
      samp_type = "Simple random sampling",
      stratified = "Not stratified",
      col_psu = "None",
      strata = "None",
      colpop = "None",
      conf_level = 0.95,
      e_marg = 0.05,
      pror = 0.5,
      buf = 0.05,
      target = 10,
      cls = 5,
      ICC = 0.06,
      seed_value = 1
    )
    session$setInputs(f_apply = 1)
    expect_false(applied())
    expect_match(output$apply_error$html, "PST_prob", fixed = TRUE)
    expect_match(output$apply_error$html, "PST_id_sample", fixed = TRUE)
  })
})

test_that("the downloads have no unnamed row-number column", {
  withr::local_dir(app_root)
  server <- source("server.R")$value

  shiny::testServer(server, {
    session$setInputs(
      data_source = "test",
      topup = "Sample size based on population",
      samp_type = "Simple random sampling",
      stratified = "Not stratified",
      col_psu = "None",
      strata = "None",
      colpop = "None",
      conf_level = 0.95,
      e_marg = 0.05,
      pror = 0.5,
      buf = 0.05,
      target = NA,
      cls = 5,
      ICC = 0.06,
      seed_value = 42
    )
    session$setInputs(f_apply = 1)
    session$setInputs(desButton = 1)

    sample_file <- readLines(output$downloadBtn, n = 1)
    summary_file <- readLines(output$downloadBtn2, n = 1)
    expect_match(sample_file, '^"PST_id_sample"')
    expect_false(startsWith(sample_file, '""'))
    expect_match(summary_file, '^"Stratum"')
  })
})
