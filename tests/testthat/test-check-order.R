# The order of the checks at Apply: the problem to fix first is reported first.
# The checks of the columns need the file, so a problem with the file comes before
# them whatever the other settings are (issue #50), and the checks of the columns
# come before those of the parameters.

# the check a message comes from, by comparing it with the templates of the file
check_of <- function(html) {
  text <- trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", html)))
  if (!nzchar(text)) {
    return("none")
  }
  first_line <- function(check) {
    strsplit(fill_field_labels(check$message), "\n", fixed = TRUE)[[1]][1]
  }
  pattern <- function(check) {
    escaped <- gsub(
      "([.|()\\^$*+?\\[\\]\\\\])",
      "\\\\\\1",
      first_line(check),
      perl = TRUE
    )
    paste0("^", gsub("\\{[a-z_]+\\}", ".*", escaped))
  }
  patterns <- vapply(CHECKS$checks, pattern, character(1))
  found <- names(patterns)[vapply(
    patterns,
    function(p) grepl(p, text),
    logical(1)
  )]
  if (length(found) == 0) {
    return(paste("unknown:", text))
  }
  found[which.max(nchar(patterns[found]))]
}

types <- c(SAMPLING_TYPE_SIMPLE, SAMPLING_TYPE_PPS, SAMPLING_TYPE_CLUSTER)
modes <- c(SAMPLING_MODE_ENTER_SIZE, SAMPLING_MODE_FROM_POPULATION)

# settings with a problem in each of the three families, one parameter invalid
settings_for <- function(
  type,
  mode,
  stratified,
  strata,
  psu,
  population,
  bad_parameter
) {
  list(
    sampling_mode = mode,
    sampling_type = type,
    stratified = stratified,
    strata_column = strata,
    psu_column = psu,
    population_column = population,
    confidence_level = if (
      bad_parameter && mode == SAMPLING_MODE_FROM_POPULATION
    ) {
      5
    } else {
      0.95
    },
    error_margin = 0.05,
    proportion = 0.5,
    buffer = 0.05,
    target_size = if (mode == SAMPLING_MODE_ENTER_SIZE) {
      if (bad_parameter) NA else 100
    } else {
      NA
    },
    cluster_size = 5,
    icc = 0.06,
    seed = 42
  )
}

first_check <- function(server, source, settings) {
  found <- NULL
  shiny::testServer(server, {
    do.call(session$setInputs, c(source, settings))
    session$setInputs(apply_button = 1)
    html <- tryCatch(output$apply_error$html, error = function(e) "")
    found <<- check_of(if (is.null(html)) "" else html)
  })
  found
}

test_that("a problem with the file is reported before any problem of the columns or the parameters", {
  withr::local_dir(app_root)
  server <- source("server.R")$value
  not_csv <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("a,b", not_csv)
  sources <- list(
    no_file = list(list(data_source = DATA_SOURCE_UPLOAD), "no_file"),
    not_csv = list(
      list(
        data_source = DATA_SOURCE_UPLOAD,
        frame_file = list(name = "frame.xlsx", datapath = not_csv)
      ),
      "csv_format"
    )
  )
  for (name in names(sources)) {
    for (type in types) {
      for (mode in modes) {
        for (stratified in c(FALSE, TRUE)) {
          for (bad in c(FALSE, TRUE)) {
            settings <- settings_for(
              type,
              mode,
              stratified,
              "None",
              "None",
              "None",
              bad
            )
            expect_identical(
              first_check(server, sources[[name]][[1]], settings),
              sources[[name]][[2]],
              info = paste(
                name,
                type,
                mode,
                "stratified",
                stratified,
                "bad parameter",
                bad
              )
            )
          }
        }
      }
    }
  }
})

test_that("with a valid file, the columns are reported in the order of the form, before the parameters", {
  withr::local_dir(app_root)
  server <- source("server.R")$value
  source <- list(data_source = DATA_SOURCE_TEST)
  cluster <- SAMPLING_TYPE_CLUSTER
  pps <- SAMPLING_TYPE_PPS
  by_mode <- SAMPLING_MODE_FROM_POPULATION

  cases <- list(
    # strata, then PSU, then population, then the parameters
    list(
      settings_for(cluster, by_mode, TRUE, "None", "None", "None", TRUE),
      "strata_missing"
    ),
    list(
      settings_for(cluster, by_mode, TRUE, "nope", "None", "None", TRUE),
      "strata_not_found"
    ),
    list(
      settings_for(cluster, by_mode, TRUE, "stratum", "None", "None", TRUE),
      "psu_missing"
    ),
    list(
      settings_for(cluster, by_mode, TRUE, "stratum", "nope", "None", TRUE),
      "psu_not_found"
    ),
    list(
      settings_for(cluster, by_mode, TRUE, "stratum", "psu_id", "None", TRUE),
      "population_missing"
    ),
    list(
      settings_for(
        cluster,
        by_mode,
        TRUE,
        "stratum",
        "psu_id",
        "stratum",
        TRUE
      ),
      "population_not_numeric"
    ),
    list(
      settings_for(pps, by_mode, TRUE, "None", "None", "None", TRUE),
      "strata_missing"
    ),
    list(
      settings_for(pps, by_mode, FALSE, "None", "None", "None", TRUE),
      "population_missing"
    ),
    # everything about the frame is fine: the parameters are reported
    list(
      settings_for(
        cluster,
        by_mode,
        TRUE,
        "stratum",
        "psu_id",
        "population",
        TRUE
      ),
      "confidence_level"
    ),
    list(
      settings_for(
        cluster,
        SAMPLING_MODE_ENTER_SIZE,
        TRUE,
        "stratum",
        "psu_id",
        "population",
        TRUE
      ),
      "target_size"
    ),
    # and nothing is reported when nothing is wrong
    list(
      settings_for(
        cluster,
        by_mode,
        TRUE,
        "stratum",
        "psu_id",
        "population",
        FALSE
      ),
      "none"
    )
  )
  for (case in cases) {
    expect_identical(
      first_check(server, source, case[[1]]),
      case[[2]],
      info = case[[2]]
    )
  }
})
