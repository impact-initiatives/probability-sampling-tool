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
type_inputs <- function(type, stratified = FALSE, strata = "Region") {
  c(
    list(
      samp_type = golden_types[[type]],
      col_psu = if (type == "cluster") "VDC_code" else "None",
      colpop = if (type == "simple") "None" else "HH_total"
    ),
    if (stratified) {
      list(stratified = "Stratified", strata = strata)
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
            list(buf = if (size == "within") 0.05 else 5)
          } else {
            list(target = if (size == "within") 100 else 3000)
          }
          add(do.call(
            scenario,
            c(
              list(id = paste(mode, type, strat, size, sep = "|")),
              list(topup = golden_modes[[mode]]),
              type_inputs(type, strat == "stratified"),
              sizes
            )
          ))
        }
      }
    }
  }

  # 2. other valid parameters
  p <- function(id, type, ..., stratified = FALSE, strata = "Region") {
    add(do.call(
      scenario,
      c(
        list(id = paste0("param|", id)),
        type_inputs(type, stratified, strata),
        list(...)
      )
    ))
  }
  p("cluster-size-2", "cluster", cls = 2, stratified = TRUE)
  p("cluster-size-12", "cluster", cls = 12, stratified = TRUE)
  p("icc-0", "cluster", ICC = 0, stratified = TRUE)
  p("icc-1", "cluster", ICC = 1, stratified = TRUE)
  p("icc-half", "cluster", ICC = 0.5)
  p("cluster-zone", "cluster", stratified = TRUE, strata = "Zone_name")
  p("cluster-district", "cluster", stratified = TRUE, strata = "District_name")
  p(
    "cluster-entered",
    "cluster",
    topup = golden_modes[["entered"]],
    target = 60,
    cls = 3,
    stratified = TRUE
  )
  p(
    "cluster-pop-other-column",
    "cluster",
    colpop = "Ind_total",
    stratified = TRUE
  )
  p("pps-other-column", "pps", colpop = "Ind_total", stratified = TRUE)
  p("pps-district", "pps", stratified = TRUE, strata = "District_name")
  p(
    "pps-entered",
    "pps",
    topup = golden_modes[["entered"]],
    target = 50,
    stratified = TRUE
  )
  p("simple-district", "simple", stratified = TRUE, strata = "District_name")
  p(
    "simple-zone-90",
    "simple",
    conf_level = 0.9,
    e_marg = 0.1,
    stratified = TRUE,
    strata = "Zone_name"
  )
  p("simple-99", "simple", conf_level = 0.99, e_marg = 0.02)
  p("simple-no-buffer", "simple", buf = 0)
  p(
    "simple-entered",
    "simple",
    topup = golden_modes[["entered"]],
    target = 50,
    stratified = TRUE
  )
  p("simple-seed-1", "simple", seed_value = 1, stratified = TRUE)
  p("simple-seed-negative", "simple", seed_value = -2147483647)
  p("pps-seed-2024", "pps", seed_value = 2024)

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
  e("strata-missing", stratified = "Stratified", strata = "None")
  e("reserved", frame = function(d) {
    d$PST_prob <- 1
    d$PST_notes <- "x"
    d
  })
  e("psu-missing", samp_type = golden_types[["cluster"]], colpop = "HH_total")
  e(
    "psu-not-found",
    samp_type = golden_types[["cluster"]],
    col_psu = "nope",
    colpop = "HH_total"
  )
  e(
    "psu-duplicates",
    samp_type = golden_types[["cluster"]],
    col_psu = "Zone_name",
    colpop = "HH_total"
  )
  e("pop-missing", samp_type = golden_types[["pps"]])
  e(
    "pop-missing-cluster",
    samp_type = golden_types[["cluster"]],
    col_psu = "VDC_code"
  )
  e("pop-not-found", samp_type = golden_types[["pps"]], colpop = "nope")
  e("pop-not-numeric", samp_type = golden_types[["pps"]], colpop = "P_CODE")
  e(
    "cluster-is-pop",
    samp_type = golden_types[["cluster"]],
    col_psu = "VDC_code",
    colpop = "VDC_code"
  )
  e(
    "cluster-is-strata",
    samp_type = golden_types[["cluster"]],
    col_psu = "VDC_code",
    colpop = "HH_total",
    stratified = "Stratified",
    strata = "VDC_code"
  )
  e(
    "strata-is-pop",
    samp_type = golden_types[["pps"]],
    colpop = "HH_total",
    stratified = "Stratified",
    strata = "HH_total"
  )
  e(
    "pop-na",
    samp_type = golden_types[["pps"]],
    colpop = "HH_total",
    frame = function(d) {
      d$HH_total[c(3, 10)] <- NA
      d
    }
  )
  e(
    "pop-negative",
    samp_type = golden_types[["pps"]],
    colpop = "HH_total",
    frame = function(d) {
      d$HH_total[c(5, 6, 7)] <- -1
      d
    }
  )
  e(
    "pop-na-and-negative-cluster",
    samp_type = golden_types[["cluster"]],
    col_psu = "VDC_code",
    colpop = "HH_total",
    frame = function(d) {
      d$HH_total[2] <- NA
      d$HH_total[4] <- -5
      d
    }
  )
  e(
    "pop-zero-psu",
    samp_type = golden_types[["cluster"]],
    col_psu = "VDC_code",
    colpop = "HH_total",
    frame = function(d) {
      d$HH_total[1] <- 0
      d
    }
  )
  e(
    "many-pop-problems",
    samp_type = golden_types[["pps"]],
    colpop = "HH_total",
    frame = function(d) {
      d$HH_total[1:9] <- NA
      d
    }
  )

  # 5. the parameters
  q <- function(id, ...) add(scenario(paste0("parameters|", id), ...))
  entered <- list(topup = golden_modes[["entered"]])
  for (target in list(NA, 0, 1.5, -3, Inf)) {
    do.call(
      q,
      c(
        list(id = paste0("target-", format(target))),
        entered,
        list(target = target)
      )
    )
  }
  for (v in list(NA, 0, 1, 1.5, -1)) {
    q(paste0("conf-", format(v)), conf_level = v)
    q(paste0("margin-", format(v)), e_marg = v)
  }
  for (v in list(NA, -1)) {
    q(paste0("buffer-", format(v)), buf = v)
  }
  for (v in list(NA, 1, 2.5, 0, -2)) {
    q(
      paste0("cls-", format(v)),
      samp_type = golden_types[["cluster"]],
      col_psu = "VDC_code",
      colpop = "HH_total",
      cls = v
    )
  }
  for (v in list(NA, -0.1, 1.5)) {
    q(
      paste0("icc-", format(v)),
      samp_type = golden_types[["cluster"]],
      col_psu = "VDC_code",
      colpop = "HH_total",
      ICC = v
    )
  }
  # the parameters of the mode that is not selected are not checked
  q(
    "conf-ignored-when-entered",
    topup = golden_modes[["entered"]],
    target = 20,
    conf_level = NA
  )
  q("icc-ignored-when-not-cluster", ICC = NA)
  for (v in list(NA, 1.5, 3e9, -3e9)) {
    q(paste0("seed-", format(v)), seed_value = v)
  }

  # 6. the cluster guards at Apply
  g <- function(id, ...) add(scenario(paste0("cluster-guard|", id), ...))
  cl <- type_inputs("cluster", TRUE)
  g(
    "psu-too-small",
    samp_type = cl$samp_type,
    col_psu = "VDC_code",
    colpop = "HH_total",
    cls = 400
  )
  g(
    "stratum-capacity",
    samp_type = cl$samp_type,
    col_psu = "VDC_code",
    colpop = "HH_total",
    stratified = "Stratified",
    strata = "Region",
    topup = golden_modes[["entered"]],
    target = 400000
  )
  g(
    "both-problems",
    samp_type = cl$samp_type,
    col_psu = "VDC_code",
    colpop = "HH_total",
    stratified = "Stratified",
    strata = "District_name",
    cls = 150
  )
  g(
    "many-small-psus",
    samp_type = cl$samp_type,
    col_psu = "VDC_code",
    colpop = "HH_total",
    cls = 130
  )
  g(
    "one-stratum-capacity-entered",
    samp_type = cl$samp_type,
    col_psu = "VDC_code",
    colpop = "HH_total",
    topup = golden_modes[["entered"]],
    target = 100000
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
      samp_type = golden_types[["cluster"]],
      col_psu = "VDC_code",
      colpop = "HH_total",
      topup = golden_modes[["entered"]],
      target = 40,
      seed_value = seed,
      frame = few_psus
    ))
  }

  # 8. several problems at once: which message comes first. Each pair is two
  #    adjacent checks of Apply, in the order of the server
  m <- function(id, ..., frame = NULL) {
    add(scenario(paste0("order|", id), ..., frame = frame))
  }
  cluster <- list(samp_type = golden_types[["cluster"]])
  m(
    "strata-then-file",
    stratified = "Stratified",
    strata = "None",
    no_file = TRUE
  )
  m("file-then-reserved", file = list(name = "frame.csv", content = ""))
  m(
    "reserved-then-psu",
    samp_type = cluster$samp_type,
    colpop = "HH_total",
    frame = function(d) {
      d$PST_a <- 1
      d
    }
  )
  m(
    "psu-then-population",
    samp_type = cluster$samp_type,
    col_psu = "Zone_name",
    colpop = "P_CODE"
  )
  m("psu-missing-then-population-missing", samp_type = cluster$samp_type)
  m(
    "population-then-issues",
    samp_type = golden_types[["pps"]],
    colpop = "P_CODE",
    frame = function(d) {
      d$HH_total[2] <- NA
      d
    }
  )
  m(
    "issues-then-parameters",
    samp_type = golden_types[["pps"]],
    colpop = "HH_total",
    conf_level = 5,
    frame = function(d) {
      d$HH_total[2] <- NA
      d
    }
  )
  m("parameters-then-seed", conf_level = 5, seed_value = NA)
  m("conf-then-margin-then-buffer", conf_level = 5, e_marg = 5, buf = -1)
  m("margin-then-buffer", e_marg = 5, buf = -1)
  m(
    "buffer-then-cluster-size",
    samp_type = cluster$samp_type,
    col_psu = "VDC_code",
    colpop = "HH_total",
    buf = -1,
    cls = 1
  )
  m(
    "cluster-size-then-icc",
    samp_type = cluster$samp_type,
    col_psu = "VDC_code",
    colpop = "HH_total",
    cls = 1,
    ICC = 5
  )
  m(
    "target-then-cluster-size",
    topup = golden_modes[["entered"]],
    samp_type = cluster$samp_type,
    col_psu = "VDC_code",
    colpop = "HH_total",
    cls = 1
  )
  m(
    "seed-then-guard",
    samp_type = cluster$samp_type,
    col_psu = "VDC_code",
    colpop = "HH_total",
    cls = 400,
    seed_value = NA
  )
  m(
    "strata-then-psu-then-population",
    stratified = "Stratified",
    strata = "None",
    samp_type = cluster$samp_type
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
    inputs$popdata <- list(name = spec$file$name, datapath = upload_path)
  } else if (!is.null(spec$frame)) {
    data.table::fwrite(spec$frame(data.table::fread(data_path)), upload_path)
    inputs$data_source <- "upload"
    inputs$popdata <- list(name = "frame.csv", datapath = upload_path)
  } else if (!is.null(spec$too_large)) {
    inputs$data_source <- "upload"
    inputs$popdata_too_large <- spec$too_large
  } else if (isTRUE(spec$no_file)) {
    inputs$data_source <- "upload"
  }

  rec <- new.env()
  shiny::testServer(server, {
    do.call(session$setInputs, inputs)
    session$setInputs(f_apply = 1)

    rec$file_error <- html_of(output$file_error)
    rec$data_status <- html_of(output$data_status)
    rec$apply_error <- html_of(output$apply_error)
    rec$applied <- applied()
    rec$frame <- digest_table(settle(frame()))
    rec$targets <- as_plain(settle(cible()))
    rec$issue_list <- as_plain(issue_list())
    rec$target_table <- rlang::hash(settle(output$target_frame))
    rec$frame_table <- rlang::hash(settle(output$sampling_frame))
    rec$exceeds <- if (
      isTRUE(applied()) && inputs$samp_type != golden_types[["cluster"]]
    ) {
      check_target_vs_population(cible())
    }

    if (isTRUE(applied())) {
      session$setInputs(desButton = 1)
      rec$sample_current <- sample_current()
      rec$draw_failed <- draw_failed()
      rec$sample_error <- html_of(output$sample_error)
      rec$draw_issues <- as_plain(draw_issues())
      rec$sample_file <- digest_lines(settle(readLines(output$downloadBtn)))
      rec$summary_file <- settle(readLines(output$downloadBtn2))
      rec$sample_table <- rlang::hash(settle(output$sampling_output))
      rec$summary_table <- rlang::hash(settle(output$test_1))
    }
  })
  mget(sort(ls(rec)), envir = rec)
}
