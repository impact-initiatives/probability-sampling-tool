# Golden master of the app (issue #36): the scenarios, and how one is run.
#
# A scenario is a set of inputs, optionally a modified or broken file. It is
# replayed on the real server (shiny::testServer): Apply, then Sample when
# Apply succeeded. The record keeps what the user would see: the messages, the
# tables, the downloaded CSV files and the lists of problems. The reference
# was recorded before the restructuring; test-golden-master.R compares to it.
#
# All the names of the inputs and of the reactives of the server are used in
# this file only: when they are renamed, change them here.

golden_base <- list(
  data_source = "test",
  sampling_mode = "Sample size based on population",
  sampling_type = "Simple random sampling",
  stratified = "Not stratified",
  psu_column = "None",
  strata_column = "None",
  population_column = "None",
  confidence_level = 0.95,
  error_margin = 0.05,
  proportion = 0.5,
  buffer = 0.05,
  target_size = NA,
  cluster_size = 5,
  icc = 0.06,
  seed = 42
)

golden_types <- c(
  simple = "Simple random sampling",
  pps = "PPS sampling with replacement",
  cluster = "Two-stage PPS cluster sampling with replacement"
)
golden_modes <- c(
  population = "Sample size based on population",
  entered = "Enter sample size"
)

# one scenario: `...` are the inputs that differ from golden_base; `frame` is a
# function changing the example data; `file` a file sent as is
scenario <- function(
  id,
  ...,
  frame = NULL,
  file = NULL,
  too_large = NULL,
  no_file = FALSE
) {
  list(
    id = id,
    inputs = list(...),
    frame = frame,
    file = file,
    too_large = too_large,
    no_file = no_file
  )
}

# the inputs a sampling type needs
type_inputs <- function(type, stratified = FALSE, strata_column = "Region") {
  c(
    list(
      sampling_type = golden_types[[type]],
      psu_column = if (type == "cluster") "VDC_code" else "None",
      population_column = if (type == "simple") "None" else "HH_total"
    ),
    if (stratified) {
      list(stratified = "Stratified", strata_column = strata_column)
    }
  )
}

golden_scenarios <- function() {
  out <- list()
  add <- function(spec) out[[spec$id]] <<- spec

  # 1. every mode x type x stratification, with a target inside the population
  #    and one far above it
  for (mode in names(golden_modes)) {
    for (type in names(golden_types)) {
      for (strat in c("unstratified", "stratified")) {
        for (size in c("within", "exceeds")) {
          sizes <- if (mode == "population") {
            list(buffer = if (size == "within") 0.05 else 5)
          } else {
            list(target_size = if (size == "within") 100 else 3000)
          }
          add(do.call(
            scenario,
            c(
              list(id = paste(mode, type, strat, size, sep = "|")),
              list(sampling_mode = golden_modes[[mode]]),
              type_inputs(type, strat == "stratified"),
              sizes
            )
          ))
        }
      }
    }
  }

  # 2. other valid parameters
  p <- function(id, type, ..., stratified = FALSE, strata_column = "Region") {
    add(do.call(
      scenario,
      c(
        list(id = paste0("param|", id)),
        type_inputs(type, stratified, strata_column),
        list(...)
      )
    ))
  }
  p("cluster-size-2", "cluster", cluster_size = 2, stratified = TRUE)
  p("cluster-size-12", "cluster", cluster_size = 12, stratified = TRUE)
  p("icc-0", "cluster", icc = 0, stratified = TRUE)
  p("icc-1", "cluster", icc = 1, stratified = TRUE)
  p("icc-half", "cluster", icc = 0.5)
  p("cluster-zone", "cluster", stratified = TRUE, strata_column = "Zone_name")
  p(
    "cluster-district",
    "cluster",
    stratified = TRUE,
    strata_column = "District_name"
  )
  p(
    "cluster-entered",
    "cluster",
    sampling_mode = golden_modes[["entered"]],
    target_size = 60,
    cluster_size = 3,
    stratified = TRUE
  )
  p(
    "cluster-pop-other-column",
    "cluster",
    population_column = "Ind_total",
    stratified = TRUE
  )
  p(
    "pps-other-column",
    "pps",
    population_column = "Ind_total",
    stratified = TRUE
  )
  p("pps-district", "pps", stratified = TRUE, strata_column = "District_name")
  p(
    "pps-entered",
    "pps",
    sampling_mode = golden_modes[["entered"]],
    target_size = 50,
    stratified = TRUE
  )
  p(
    "simple-district",
    "simple",
    stratified = TRUE,
    strata_column = "District_name"
  )
  p(
    "simple-zone-90",
    "simple",
    confidence_level = 0.9,
    error_margin = 0.1,
    stratified = TRUE,
    strata_column = "Zone_name"
  )
  p("simple-99", "simple", confidence_level = 0.99, error_margin = 0.02)
  p("simple-no-buffer", "simple", buffer = 0)
  p(
    "simple-entered",
    "simple",
    sampling_mode = golden_modes[["entered"]],
    target_size = 50,
    stratified = TRUE
  )
  p("simple-seed-1", "simple", seed = 1, stratified = TRUE)
  p("simple-seed-negative", "simple", seed = -2147483647)
  p("pps-seed-2024", "pps", seed = 2024)

  # 3. the file
  add(scenario("file|no-file", no_file = TRUE))
  add(scenario("file|too-large", too_large = "big_frame.csv"))
  add(scenario("file|too-large-not-csv", too_large = "big_frame.xlsx"))
  add(scenario(
    "file|not-csv",
    file = list(name = "frame.xlsx", content = "a,b\n1,2\n")
  ))
  add(scenario("file|empty", file = list(name = "frame.csv", content = "")))
  add(scenario(
    "file|ragged",
    file = list(
      name = "frame.csv",
      content = "a,b,c\n1,2,3\n4,5,6,7,8\n1,2,3\n"
    )
  ))
  add(scenario(
    "file|header-only",
    file = list(name = "frame.csv", content = "a,b\n")
  ))
  add(scenario("file|upload-ok", frame = function(d) d[1:200]))

  # 4. the columns
  e <- function(id, ..., frame = NULL) {
    add(scenario(paste0("columns|", id), ..., frame = frame))
  }
  e("strata-missing", stratified = "Stratified", strata_column = "None")
  e("reserved", frame = function(d) {
    d$PST_prob <- 1
    d$PST_notes <- "x"
    d
  })
  e(
    "psu-missing",
    sampling_type = golden_types[["cluster"]],
    population_column = "HH_total"
  )
  e(
    "psu-not-found",
    sampling_type = golden_types[["cluster"]],
    psu_column = "nope",
    population_column = "HH_total"
  )
  e(
    "psu-duplicates",
    sampling_type = golden_types[["cluster"]],
    psu_column = "Zone_name",
    population_column = "HH_total"
  )
  e("pop-missing", sampling_type = golden_types[["pps"]])
  e(
    "pop-missing-cluster",
    sampling_type = golden_types[["cluster"]],
    psu_column = "VDC_code"
  )
  e(
    "pop-not-found",
    sampling_type = golden_types[["pps"]],
    population_column = "nope"
  )
  e(
    "pop-not-numeric",
    sampling_type = golden_types[["pps"]],
    population_column = "P_CODE"
  )
  e(
    "cluster-is-pop",
    sampling_type = golden_types[["cluster"]],
    psu_column = "VDC_code",
    population_column = "VDC_code"
  )
  e(
    "cluster-is-strata",
    sampling_type = golden_types[["cluster"]],
    psu_column = "VDC_code",
    population_column = "HH_total",
    stratified = "Stratified",
    strata_column = "VDC_code"
  )
  e(
    "strata-is-pop",
    sampling_type = golden_types[["pps"]],
    population_column = "HH_total",
    stratified = "Stratified",
    strata_column = "HH_total"
  )
  e(
    "pop-na",
    sampling_type = golden_types[["pps"]],
    population_column = "HH_total",
    frame = function(d) {
      d$HH_total[c(3, 10)] <- NA
      d
    }
  )
  e(
    "pop-negative",
    sampling_type = golden_types[["pps"]],
    population_column = "HH_total",
    frame = function(d) {
      d$HH_total[c(5, 6, 7)] <- -1
      d
    }
  )
  e(
    "pop-na-and-negative-cluster",
    sampling_type = golden_types[["cluster"]],
    psu_column = "VDC_code",
    population_column = "HH_total",
    frame = function(d) {
      d$HH_total[2] <- NA
      d$HH_total[4] <- -5
      d
    }
  )
  e(
    "pop-zero-psu",
    sampling_type = golden_types[["cluster"]],
    psu_column = "VDC_code",
    population_column = "HH_total",
    frame = function(d) {
      d$HH_total[1] <- 0
      d
    }
  )
  e(
    "many-pop-problems",
    sampling_type = golden_types[["pps"]],
    population_column = "HH_total",
    frame = function(d) {
      d$HH_total[1:9] <- NA
      d
    }
  )

  # 5. the parameters
  q <- function(id, ...) add(scenario(paste0("parameters|", id), ...))
  entered <- list(sampling_mode = golden_modes[["entered"]])
  for (target_size in list(NA, 0, 1.5, -3, Inf)) {
    do.call(
      q,
      c(
        list(id = paste0("target-", format(target_size))),
        entered,
        list(target_size = target_size)
      )
    )
  }
  for (v in list(NA, 0, 1, 1.5, -1)) {
    q(paste0("conf-", format(v)), confidence_level = v)
    q(paste0("margin-", format(v)), error_margin = v)
  }
  for (v in list(NA, -1)) {
    q(paste0("buffer-", format(v)), buffer = v)
  }
  for (v in list(NA, 1, 2.5, 0, -2)) {
    q(
      paste0("cls-", format(v)),
      sampling_type = golden_types[["cluster"]],
      psu_column = "VDC_code",
      population_column = "HH_total",
      cluster_size = v
    )
  }
  for (v in list(NA, -0.1, 1.5)) {
    q(
      paste0("icc-", format(v)),
      sampling_type = golden_types[["cluster"]],
      psu_column = "VDC_code",
      population_column = "HH_total",
      icc = v
    )
  }
  # the parameters of the mode that is not selected are not checked
  q(
    "conf-ignored-when-entered",
    sampling_mode = golden_modes[["entered"]],
    target_size = 20,
    confidence_level = NA
  )
  q("icc-ignored-when-not-cluster", icc = NA)
  for (v in list(NA, 1.5, 3e9, -3e9)) {
    q(paste0("seed-", format(v)), seed = v)
  }

  # 6. the cluster guards at Apply
  g <- function(id, ...) add(scenario(paste0("cluster-guard|", id), ...))
  cl <- type_inputs("cluster", TRUE)
  g(
    "psu-too-small",
    sampling_type = cl$sampling_type,
    psu_column = "VDC_code",
    population_column = "HH_total",
    cluster_size = 400
  )
  g(
    "stratum-capacity",
    sampling_type = cl$sampling_type,
    psu_column = "VDC_code",
    population_column = "HH_total",
    stratified = "Stratified",
    strata_column = "Region",
    sampling_mode = golden_modes[["entered"]],
    target_size = 400000
  )
  g(
    "both-problems",
    sampling_type = cl$sampling_type,
    psu_column = "VDC_code",
    population_column = "HH_total",
    stratified = "Stratified",
    strata_column = "District_name",
    cluster_size = 150
  )
  g(
    "many-small-psus",
    sampling_type = cl$sampling_type,
    psu_column = "VDC_code",
    population_column = "HH_total",
    cluster_size = 130
  )
  g(
    "one-stratum-capacity-entered",
    sampling_type = cl$sampling_type,
    psu_column = "VDC_code",
    population_column = "HH_total",
    sampling_mode = golden_modes[["entered"]],
    target_size = 100000
  )

  # 7. the draw: six PSUs of 10 households host 2 clusters of 5 each, so a PSU
  #    drawn three times or more overflows; it depends on the seed
  few_psus <- function(d) {
    d <- d[1:6]
    d$HH_total <- 10
    d
  }
  for (seed in c(1, 2, 3, 42)) {
    add(scenario(
      paste0("draw|overflow-seed-", seed),
      sampling_type = golden_types[["cluster"]],
      psu_column = "VDC_code",
      population_column = "HH_total",
      sampling_mode = golden_modes[["entered"]],
      target_size = 40,
      seed = seed,
      frame = few_psus
    ))
  }

  # 8. several problems at once: which message comes first. Each pair is two
  #    adjacent checks of Apply, in the order of the server
  m <- function(id, ..., frame = NULL) {
    add(scenario(paste0("order|", id), ..., frame = frame))
  }
  cluster <- list(sampling_type = golden_types[["cluster"]])
  m(
    "strata-then-file",
    stratified = "Stratified",
    strata_column = "None",
    no_file = TRUE
  )
  m("file-then-reserved", file = list(name = "frame.csv", content = ""))
  m(
    "reserved-then-psu",
    sampling_type = cluster$sampling_type,
    population_column = "HH_total",
    frame = function(d) {
      d$PST_a <- 1
      d
    }
  )
  m(
    "psu-then-population",
    sampling_type = cluster$sampling_type,
    psu_column = "Zone_name",
    population_column = "P_CODE"
  )
  m(
    "psu-missing-then-population-missing",
    sampling_type = cluster$sampling_type
  )
  m(
    "population-then-issues",
    sampling_type = golden_types[["pps"]],
    population_column = "P_CODE",
    frame = function(d) {
      d$HH_total[2] <- NA
      d
    }
  )
  m(
    "issues-then-parameters",
    sampling_type = golden_types[["pps"]],
    population_column = "HH_total",
    confidence_level = 5,
    frame = function(d) {
      d$HH_total[2] <- NA
      d
    }
  )
  m("parameters-then-seed", confidence_level = 5, seed = NA)
  m(
    "conf-then-margin-then-buffer",
    confidence_level = 5,
    error_margin = 5,
    buffer = -1
  )
  m("margin-then-buffer", error_margin = 5, buffer = -1)
  m(
    "buffer-then-cluster-size",
    sampling_type = cluster$sampling_type,
    psu_column = "VDC_code",
    population_column = "HH_total",
    buffer = -1,
    cluster_size = 1
  )
  m(
    "cluster-size-then-icc",
    sampling_type = cluster$sampling_type,
    psu_column = "VDC_code",
    population_column = "HH_total",
    cluster_size = 1,
    icc = 5
  )
  m(
    "target-then-cluster-size",
    sampling_mode = golden_modes[["entered"]],
    sampling_type = cluster$sampling_type,
    psu_column = "VDC_code",
    population_column = "HH_total",
    cluster_size = 1
  )
  m(
    "seed-then-guard",
    sampling_type = cluster$sampling_type,
    psu_column = "VDC_code",
    population_column = "HH_total",
    cluster_size = 400,
    seed = NA
  )
  m(
    "strata-then-psu-then-population",
    stratified = "Stratified",
    strata_column = "None",
    sampling_type = cluster$sampling_type
  )

  out
}

# --- how a scenario is run and recorded --------------------------------------

# the value of `x`, or the text of the validation message / error that stopped it
settle <- function(x) {
  tryCatch(
    x,
    shiny.silent.error = function(e) paste0("<silent> ", conditionMessage(e)),
    error = function(e) paste0("<error> ", conditionMessage(e))
  )
}

html_of <- function(x) {
  v <- settle(x)
  if (is.character(v)) {
    v
  } else if (is.null(v)) {
    "<null>"
  } else {
    as.character(v$html)
  }
}

# the columns of a table, without the attributes of data.table
digest_table <- function(x) {
  if (!is.data.frame(x)) {
    return(x)
  }
  list(
    nrow = nrow(x),
    names = names(x),
    hash = rlang::hash(lapply(names(x), function(n) x[[n]]))
  )
}

digest_lines <- function(x) {
  if (!is.character(x) || length(x) < 2) {
    return(x)
  }
  list(n = length(x), header = x[1], hash = rlang::hash(x))
}

as_plain <- function(x) {
  if (is.data.frame(x)) as.data.frame(x) else x
}

run_golden_scenario <- function(spec, server, data_path) {
  inputs <- utils::modifyList(golden_base, spec$inputs)
  # modifyList() drops NULL, not NA: an NA stays an NA
  upload_path <- withr::local_tempfile(fileext = ".csv")

  if (!is.null(spec$file)) {
    writeBin(charToRaw(spec$file$content), upload_path)
    inputs$data_source <- "upload"
    inputs$frame_file <- list(name = spec$file$name, datapath = upload_path)
  } else if (!is.null(spec$frame)) {
    data.table::fwrite(spec$frame(data.table::fread(data_path)), upload_path)
    inputs$data_source <- "upload"
    inputs$frame_file <- list(name = "frame.csv", datapath = upload_path)
  } else if (!is.null(spec$too_large)) {
    inputs$data_source <- "upload"
    inputs$rejected_file_name <- spec$too_large
  } else if (isTRUE(spec$no_file)) {
    inputs$data_source <- "upload"
  }

  rec <- new.env()
  shiny::testServer(server, {
    do.call(session$setInputs, inputs)
    session$setInputs(apply_button = 1)

    rec$file_error <- html_of(output$file_error)
    rec$data_status <- html_of(output$data_status)
    rec$apply_error <- html_of(output$apply_error)
    rec$applied <- applied()
    rec$frame <- digest_table(settle(sampling_frame()))
    rec$targets <- as_plain(settle(targets()))
    rec$issue_list <- as_plain(apply_issues())
    rec$target_table <- rlang::hash(settle(output$target_table))
    rec$frame_table <- rlang::hash(settle(output$frame_table))
    rec$exceeds <- if (
      isTRUE(applied()) && inputs$sampling_type != golden_types[["cluster"]]
    ) {
      check_target_vs_population(targets())
    }

    if (isTRUE(applied())) {
      session$setInputs(draw_button = 1)
      rec$sample_current <- sample_current()
      rec$draw_failed <- draw_failed()
      rec$sample_error <- html_of(output$sample_error)
      rec$draw_issues <- as_plain(draw_issues())
      rec$sample_file <- digest_lines(settle(readLines(output$download_sample)))
      rec$summary_file <- settle(readLines(output$download_summary))
      rec$sample_table <- rlang::hash(settle(output$sample_table))
      rec$summary_table <- rlang::hash(settle(output$summary_table))
    }
  })
  mget(sort(ls(rec)), envir = rec)
}
