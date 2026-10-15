# This script contains functions related to probability sampling.
# It includes functions for creating a sampling frame, performing cluster sampling, random sampling, and two-stage random sampling.
# The functions are used to simulate samples based on user-defined parameters.

require(data.table)
require(dplyr)
require(car)
require(reshape2)
require(stringr)
library(shiny)
library(shinyjs)
library(crayon)

# upload size limit, checked in the browser; Shiny refuses larger requests
# before the server sees them, hence the margin for the multipart overhead
max_upload_mb <- 100
options(shiny.maxRequestSize = (max_upload_mb + 1) * 1024^2)


# humanTime function returns the current time in a specific format.
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# Runs `expr` under a given RNG seed, restoring the previous RNG state
# afterwards so the seed doesn't affect any other randomness in the session.
run_with_seed <- function(seed, expr) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  })
  set.seed(seed)
  expr
}

# Proportion used by the sample size computation. It is fixed: 0.5 is the most
# cautious value, since p * (1 - p) is largest there and so is the sample size.
# The field stays visible, not editable, in the interface (issues #8 and #9).
fixed_proportion <- 0.5

# Calculate the sample size required to estimate a proportion
#
# Cochran's formula for a proportion (Cochran, 1977, Sampling Techniques, 3rd ed.)
# with the finite population correction:
#   n0 = z^2 * p * (1 - p) / e^2         sample size for an infinite population
#   n  = n0 / (1 + (n0 - 1) / N)         finite population correction
#      = z^2 * N * p * (1 - p) / (e^2 * (N - 1) + z^2 * p * (1 - p))
# where z is the two-sided normal quantile of the confidence level: z^2 is also
# qchisq(conf_level, df = 1). The result is then multiplied by the design effect.
#
# Parameters:
#   population_size: The population size (N)
#   conf_level: The desired level of confidence (between 0 and 1)
#   prop: The estimated population proportion (p, between 0 and 1)
#   margin_error: The desired margin of error (e)
#   DEFF: Design effect to inflate the sample size for (default 1, i.e. no inflation)
# Returns:
#   The sample size required to achieve the desired level of confidence and margin of error
Ssize <- function(population_size, conf_level, prop, margin_error, DEFF = 1) {
  z <- qnorm(1 - (1 - conf_level) / 2) # two-sided z-score for confidence level
  n0 <- (z^2 * prop * (1 - prop)) / margin_error^2 # infinite population approximation
  n <- n0 / (1 + (n0 - 1) / population_size) # finite population correction
  ceiling(n * DEFF)
}


# create the sampling frame
# This function creates a sampling frame based on the input parameters.
# It adds additional columns to the input data frame, such as PST_id_sample, PST_stratum_id, PST_psu_id, PST_pop_size, and PST_prob.
# The PST_id_sample column is created by concatenating "id_" with the row names of the input data frame.
# If the sampling method is "Stratified", the PST_stratum_id column is created by extracting the values from the specified strata column in the input data frame.
# If the sampling method is "Cluster sampling", the PST_psu_id column is created by extracting the values from the specified col_psu column in the input data frame,
# and the PST_pop_size column is created by extracting the values from the specified colpop column in the input data frame.
# If the sampling method is "Simple random - allocation" or any other method, the PST_psu_id column is created using the PST_id_sample column,
# and the PST_pop_size column is created with a value of 1 for each row.
# The PST_pop_size_stratum column is calculated by summing the PST_pop_size column within each PST_stratum_id group.
# The PST_prob column is calculated by dividing the PST_pop_size column by the PST_pop_size_stratum column.
# Any rows with missing values in the PST_prob column are removed.
# If the sampling method is "Cluster sampling", the PST_psu_id column is converted to a factor.
# The resulting data frame is returned.
format_sampling_frame <- function(sframe, input) {
  sframe$PST_id_sample <- paste0("id_", rownames(sframe))
  if (input$stratified == "Stratified") {
    sframe$PST_stratum_id <- sframe[[as.character(input$strata)]]
  } else {
    sframe$PST_stratum_id <- rep("all", nrow(sframe))
  }

  if (input$samp_type == "Cluster sampling") {
    sframe$PST_psu_id <- sframe[[as.character(input$col_psu)]]
    sframe$PST_pop_size <- sframe[[as.character(input$colpop)]]
  } else if (input$samp_type == "Simple random - allocation") {
    sframe$PST_psu_id <- sframe$PST_id_sample
    sframe$PST_pop_size <- sframe[[as.character(input$colpop)]]
  } else {
    sframe$PST_psu_id <- sframe$PST_id_sample
    sframe$PST_pop_size <- rep(1, nrow(sframe))
  }

  sumdist <- sframe %>%
    dplyr::group_by(PST_stratum_id) %>%
    dplyr::summarise(PST_pop_size_stratum = sum(PST_pop_size, na.rm = T))
  sframe <- merge(sframe, sumdist, by = "PST_stratum_id")
  PST_prob <- as.numeric(sframe$PST_pop_size) / as.numeric(sframe$PST_pop_size_stratum)
  # an empty PSU alone in its stratum gives 0/0: keep it so that the cluster
  # size guard reports it instead of the row vanishing from the frame
  if (input$samp_type == "Cluster sampling") {
    PST_prob[is.nan(PST_prob)] <- 0
  }
  sframe <- cbind(sframe, PST_prob)
  sframe <- sframe[!is.na(sframe$PST_prob), ]

  if (input$samp_type == "Cluster sampling") {
    sframe$PST_psu_id <- as.factor(sframe$PST_psu_id)
  }
  return(sframe)
}


# Check that a stratification column has been selected when Stratified mode is active.
# Returns an error message string if the input is invalid, or NULL if valid.
validate_strata_selection <- function(input) {
  if (
    input$stratified == "Stratified" &&
      (is.null(input$strata) || input$strata == "None")
  ) {
    return(
      "Please select a stratification variable, or set 'Stratified ?' to 'Not stratified'."
    )
  }
  return(NULL)
}

# Prefix of the columns the tool adds to the sampling frame and to the sample.
reserved_prefix <- "PST_"

# Check that the sampling frame has no column starting with the reserved prefix:
# it would be overwritten or break the draw. The usual cause is a previous
# sample reloaded as a sampling frame.
# Returns an error message string if one is present, or NULL if valid.
validate_reserved_columns <- function(sframe) {
  found <- names(sframe)[startsWith(names(sframe), reserved_prefix)]
  if (length(found) == 0) {
    return(NULL)
  }
  paste0(
    "The sampling frame has column(s) with protected names: ",
    paste(found, collapse = ", "),
    ".\n\nNames starting with ",
    reserved_prefix,
    " are reserved: the tool creates columns with these names during the sampling and in the sample file, so yours would be overwritten or cause an error. Rename or remove them in the CSV."
  )
}

# Check that the selected population column is numeric when required for the sampling type.
# Returns an error message string if the input is invalid, or NULL if valid.
validate_population_column <- function(sframe, input) {
  if (
    !(input$samp_type %in% c("Cluster sampling", "Simple random - allocation"))
  ) {
    return(NULL)
  }
  if (is.null(input$colpop) || input$colpop == "None") {
    return("Please select a population column for this sampling type.")
  }
  col <- as.character(input$colpop)
  if (!(col %in% names(sframe))) {
    return(paste0(
      "Population column '",
      col,
      "' was not found in the uploaded dataset."
    ))
  }
  if (!is.numeric(sframe[[col]])) {
    return(paste0(
      "'",
      col,
      "' is not a numeric column. Select a numeric population column."
    ))
  }
  if (
    input$samp_type == "Cluster sampling" &&
      identical(col, as.character(input$col_psu))
  ) {
    return("Cluster and population variables must be different columns.")
  }
  if (
    input$stratified == "Stratified" &&
      identical(col, as.character(input$strata))
  ) {
    return("Stratification and population variables must be different columns.")
  }
  return(NULL)
}


# Rows whose population is missing or negative: a missing value would silently
# drop the row out of the frame and a negative one breaks the draw.
# Returns one row per problem (`data_row` ignores the header), or NULL.
population_issues <- function(sframe, input) {
  if (
    !(input$samp_type %in% c("Cluster sampling", "Simple random - allocation"))
  ) {
    return(NULL)
  }
  values <- sframe[[as.character(input$colpop)]]
  bad <- which(is.na(values) | values < 0)
  if (length(bad) == 0) {
    return(NULL)
  }
  data.frame(
    issue = ifelse(
      is.na(values[bad]),
      "Missing population",
      "Negative population"
    ),
    data_row = bad,
    psu = if (input$samp_type == "Cluster sampling") {
      as.character(sframe[[as.character(input$col_psu)]][bad])
    } else {
      NA_character_
    },
    population = values[bad]
  )
}


# Error message for the output of population_issues(), NULL when there is none.
population_message <- function(issues, col) {
  if (is.null(issues)) {
    return(NULL)
  }
  describe <- function(label, what) {
    rows <- issues$data_row[issues$issue == label]
    if (length(rows) > 0) {
      paste0(
        "Population column '",
        col,
        "' ",
        what,
        " in ",
        length(rows),
        " row(s): ",
        preview_ids(rows),
        " (header excluded)."
      )
    }
  }
  paste0(
    paste(
      c(
        describe("Missing population", "has missing values"),
        describe("Negative population", "has negative values")
      ),
      collapse = "\n"
    ),
    "\n\nThe population must be a number greater than or equal to 0: fix these values or remove the rows from the sampling frame."
  )
}


# Check that the seed is a single finite whole number within set.seed()'s range.
# Guards against a cleared field (NA) or a decimal/out-of-range value that would
# make set.seed() throw and break the sampling reactive.
# Returns an error message string if the input is invalid, or NULL if valid.
validate_seed <- function(seed) {
  if (
    length(seed) != 1 ||
      is.na(seed) ||
      !is.finite(seed) ||
      seed != floor(seed) ||
      abs(seed) > .Machine$integer.max
  ) {
    return(
      "Seed must be a whole number between -2147483647 and 2147483647."
    )
  }
  return(NULL)
}


# Message when `name` is not a .csv file, NULL otherwise.
csv_format_error <- function(name) {
  if (tolower(tools::file_ext(name)) != "csv") {
    paste0(
      "'",
      name,
      "' is not a CSV file. Only .csv files are accepted: for an ",
      "Excel workbook, save the sheet you need as CSV first."
    )
  }
}

# Reads an uploaded file as a CSV table.
# Returns list(data, error): `data` is NULL and `error` holds a message when the
# file is not a .csv or cannot be read as a table.
read_uploaded_csv <- function(name, path) {
  fail <- function(msg) list(data = NULL, error = msg)

  format_error <- csv_format_error(name)
  if (!is.null(format_error)) {
    return(fail(format_error))
  }

  # a parse warning means rows were dropped (e.g. a ragged line): reject the file.
  # Raising an error from the warning handler would leave fread() in a bad state.
  parse_warned <- FALSE
  data <- tryCatch(
    withCallingHandlers(
      fread(path),
      warning = function(w) {
        parse_warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  if (parse_warned || is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(fail(paste0(
      "'",
      name,
      "' could not be read as a CSV table (empty or damaged file)."
    )))
  }
  list(data = data, error = NULL)
}


# TRUE for a single finite number (FALSE for NULL, NA, NaN, Inf or vectors).
is_number <- function(x) {
  length(x) == 1 && is.numeric(x) && is.finite(x)
}


# Check the sampling parameters that make the sample size computation or the
# draw fail when they are mathematically invalid. The min/max of the numeric
# inputs mirror these rules, but Shiny does not enforce them: it sends whatever
# is typed. Only the parameters
# used by the selected mode and sampling type are checked.
# Returns an error message string if one is invalid, or NULL if all are valid.
validate_parameters <- function(input) {
  strictly_between_0_and_1 <- function(x) is_number(x) && x > 0 && x < 1

  if (input$topup == "Enter sample size") {
    target <- input$target
    if (!is_number(target) || target <= 0 || target != floor(target)) {
      return("Enter a target sample size: a whole number greater than 0.")
    }
  } else {
    if (!strictly_between_0_and_1(input$conf_level)) {
      return("Confidence level must be a number strictly between 0 and 1.")
    }
    if (!strictly_between_0_and_1(input$e_marg)) {
      return("Error margin must be a number strictly between 0 and 1.")
    }
    if (!is_number(input$buf) || input$buf < 0) {
      return("Buffer must be a number greater than or equal to 0.")
    }
  }

  if (input$samp_type == "Cluster sampling") {
    cls <- input$cls
    if (!is_number(cls) || cls < 2 || cls != floor(cls)) {
      return("Cluster size must be a whole number of at least 2.")
    }
    if (!is_number(input$ICC) || input$ICC < 0 || input$ICC > 1) {
      return("ICC must be a number between 0 and 1.")
    }
  }
  return(NULL)
}


# Shows at most `n` ids, e.g. "a, b, c, ... +4 more".
preview_ids <- function(ids, n = 5) {
  ids <- as.character(ids)
  if (length(ids) <= n) {
    return(paste(ids, collapse = ", "))
  }
  paste0(paste(ids[1:n], collapse = ", "), ", ... +", length(ids) - n, " more")
}


# Cluster sampling guard, run before any draw: it depends on the data and the
# parameters only, never on the seed. It looks for
# - PSUs smaller than the cluster size: they cannot host a cluster;
# - strata that cannot host the clusters their target requires: they need
#   ceiling(target / cls) clusters but their PSUs have room for
#   sum(floor(population / cls)).
# `target` is the output of create_targets().
# Returns one row per problem (a PSU row has a `psu`, a stratum row has none),
# or NULL when there is no problem.
cluster_size_issues <- function(sampl_f, target, input) {
  if (input$samp_type != "Cluster sampling") {
    return(NULL)
  }
  cls <- input$cls
  strata <- as.character(sampl_f$PST_stratum_id)
  pop <- sampl_f$PST_pop_size
  psu_rows <- NULL
  stratum_rows <- NULL

  too_small <- !is.na(pop) & pop < cls
  if (any(too_small)) {
    psu_rows <- data.frame(
      issue = "PSU too small for the cluster size",
      stratum = strata[too_small],
      psu = as.character(sampl_f$PST_psu_id[too_small]),
      n_psus = NA_integer_,
      population = pop[too_small],
      cluster_size = cls,
      target_with_buffer = NA_real_,
      max_surveys = NA_real_,
      shortfall = NA_real_
    )
  }

  capacity <- tapply(floor(pop / cls), strata, sum, na.rm = TRUE)
  target_by_stratum <- setNames(
    target$target.with.buffer,
    as.character(target$PST_stratum_id)
  )
  needed <- ceiling(target_by_stratum[names(capacity)] / cls)
  short <- names(capacity)[capacity < needed]
  if (length(short) > 0) {
    stratum_rows <- data.frame(
      issue = "Stratum cannot reach its target with this cluster size",
      stratum = short,
      psu = NA_character_,
      n_psus = as.integer(table(strata)[short]),
      population = setNames(target$Population, as.character(target$PST_stratum_id))[
        short
      ],
      cluster_size = cls,
      target_with_buffer = target_by_stratum[short],
      max_surveys = capacity[short] * cls,
      shortfall = target_by_stratum[short] - capacity[short] * cls
    )
    stratum_rows <- stratum_rows[order(-stratum_rows$shortfall), ]
  }
  out <- rbind(psu_rows, stratum_rows)
  if (!is.null(out)) {
    rownames(out) <- NULL
  }
  out
}


# Error message for the output of cluster_size_issues(), NULL when there is none.
# Each problem has its own paragraph and only a few examples: the full list is
# downloadable.
cluster_size_message <- function(issues, cls) {
  if (is.null(issues)) {
    return(NULL)
  }
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  blocks <- character()

  psus <- issues[!is.na(issues$psu), ]
  if (nrow(psus) > 0) {
    blocks <- c(
      blocks,
      paste0(
        "PSUs smaller than the cluster size (",
        cls,
        "): ",
        nrow(psus),
        " PSU(s) cannot host a cluster - ",
        preview_ids(
          paste0(
            psus$psu,
            " in '",
            psus$stratum,
            "' (population ",
            fmt(psus$population),
            ")"
          ),
          n = 3
        ),
        ".\nReduce the cluster size or filter these PSUs out of the sampling frame."
      )
    )
  }

  strata <- issues[is.na(issues$psu), ]
  if (nrow(strata) > 0) {
    blocks <- c(
      blocks,
      paste0(
        "Insufficient stratum capacity: ",
        nrow(strata),
        " stratum(s) cannot reach their target with clusters of ",
        cls,
        " surveys. Largest shortfalls - ",
        preview_ids(
          paste0(
            "'",
            strata$stratum,
            "' (target ",
            fmt(strata$target_with_buffer),
            ", maximum ",
            fmt(strata$max_surveys),
            ")"
          ),
          n = 3
        ),
        ".\nReduce the cluster size or the target sample size, or merge PSUs."
      )
    )
  }
  paste(blocks, collapse = "\n\n")
}


# Cluster sampling draws PSUs with replacement: a PSU drawn several times is
# assigned cluster size x draws surveys, which can exceed its population. Unlike
# the guard at Apply this depends on the seed, so it is checked on the drawn
# sample (the output of make_sample()$sample).
# Returns one row per PSU concerned, or NULL when there is none.
psu_overflow_issues <- function(sample, input) {
  if (input$samp_type != "Cluster sampling") {
    return(NULL)
  }
  over <- which(sample$PST_survey > sample$PST_pop_size)
  if (length(over) == 0) {
    return(NULL)
  }
  s <- sample[over, ]
  out <- data.frame(
    issue = "PSU drawn more often than its population allows",
    stratum = as.character(s$PST_stratum_id),
    psu = as.character(s$PST_psu_id),
    times_drawn = s$PST_survey / input$cls,
    cluster_size = input$cls,
    surveys_assigned = s$PST_survey,
    population = s$PST_pop_size,
    excess = s$PST_survey - s$PST_pop_size
  )
  out[order(-out$excess), ]
}


# Error message for the output of psu_overflow_issues(), NULL when there is none.
psu_overflow_message <- function(issues) {
  if (is.null(issues)) {
    return(NULL)
  }
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  paste0(
    "Draw blocked: ",
    nrow(issues),
    " PSU(s) were selected several times and would need more surveys than their population - ",
    preview_ids(
      paste0(
        "'",
        issues$psu,
        "' in '",
        issues$stratum,
        "' (drawn ",
        issues$times_drawn,
        " times, ",
        fmt(issues$surveys_assigned),
        " surveys for a population of ",
        fmt(issues$population),
        ")"
      ),
      n = 3
    ),
    ".\nReduce the cluster size or the target sample size, or merge PSUs."
  )
}


# Check whether the requested target exceeds the available population in any stratum.
# Returns a character vector of affected stratum names, or NULL if none.
check_target_vs_population <- function(cible) {
  affected <- cible$PST_stratum_id[cible$target.with.buffer > cible$Population]
  if (length(affected) > 0) {
    return(as.character(affected))
  }
  return(NULL)
}

# Labels of the "Target sampling" table. The values are what was requested: in
# a random sampling type the draw is capped to the population of the stratum, so
# they can be larger than the population next to them.
label_target_table <- function(target) {
  names(target)[names(target) == "target"] <- "Requested Target"
  names(target)[
    names(target) == "target.with.buffer"
  ] <- "Requested Target (with buffer)"
  target
}

# Warning shown when the requested sample size exceeds the population of some
# strata (`affected`). `drawn` is TRUE once the sample has been drawn.
exceeds_population_modal <- function(affected, drawn = FALSE) {
  modalDialog(
    title = tagList(
      icon("triangle-exclamation"),
      "Sample size exceeds population"
    ),
    div(
      class = "alert alert-warning",
      paste0(
        "The requested sample size exceeds the available population in stratum(s): ",
        paste(affected, collapse = ", "),
        ". The sample ",
        if (drawn) "was" else "will be",
        " capped to the available population there."
      )
    ),
    easyClose = TRUE,
    footer = modalButton("OK")
  )
}

# Check that the PSU (cluster) column is selected, distinct from the
# stratification column, and contains unique values (no duplicate PSU IDs).
# Returns an error message string if the input is invalid, or NULL if valid.
validate_psu_column <- function(sframe, input) {
  if (input$samp_type != "Cluster sampling") {
    return(NULL)
  }
  if (is.null(input$col_psu) || input$col_psu == "None") {
    return("Please select a cluster (PSU) column.")
  }
  if (input$stratified == "Stratified" && input$col_psu == input$strata) {
    return(
      "Cluster and stratification variables must be different columns."
    )
  }
  psu_col <- as.character(input$col_psu)
  if (is.null(sframe) || !psu_col %in% names(sframe)) {
    return(paste0(
      "Input cluster '",
      psu_col,
      "' was not found in the uploaded data. Please re-select the column."
    ))
  }
  if (anyDuplicated(sframe[[psu_col]]) > 0) {
    return(paste0(
      "Input cluster '",
      input$col_psu,
      "' contains duplicate values: the sampling frame must have one row per cluster (PSU), so this column must uniquely identify each cluster."
    ))
  }
  return(NULL)
}


# Calculate the sample size required for a given population proportion
#
# This function takes in a  dataframe and an input list, and calculates the sample size required for a given population proportion.
# It creates a new column 'PST_stratum_id' in the  dataframe based on the input 'strata' value.
# It then groups the dataframe by 'PST_stratum_id' and calculates the maximum population value for each group.
# Finally, it calculates the target sample size based on the input parameters, and also calculates the target sample size with a buffer.
#
# Args:
#   sframe: A dataframe containing the sampling frame data.
#   input: A list containing the input parameters.
#
# Returns:
#   A modified version of the sampling frame dataframe with additional columns 'target' and 'target.with.buffer' representing the calculated sample sizes.

create_targets <- function(sframe, input) {
  # DEFF is fixed by the planned cluster size and ICC, computed once here so
  # that the target sample size (and the "Target sampling" tab shown to the
  # user) already reflects the design-effect-adjusted number for Cluster
  # sampling. DEFF=1 (no adjustment) for the other sampling methods.
  DEFF <- if (input$samp_type == "Cluster sampling") {
    1 + (input$cls - 1) * input$ICC
  } else {
    1
  }

  sframe |>
    dplyr::group_by(PST_stratum_id) |>
    dplyr::summarise(
      Population = sum(PST_pop_size, na.rm = T)
    ) |>
    dplyr::mutate(
      target = (if (input$topup == "Enter sample size") {
        input$target
      } else {
        Ssize(
          population_size = Population,
          conf_level = input$conf_level,
          prop = fixed_proportion,
          margin_error = input$e_marg,
          DEFF = DEFF
        )
      }) |>
        as.numeric(),
      target.with.buffer = if (input$topup == "Enter sample size") {
        target
      } else {
        as.numeric(ceiling(target * (1 + input$buf)))
      }
    )
}


# clustersample draws the PSUs of one stratum. The guard in
# validate_cluster_size() must have passed: there is no fallback.
# Parameters:
# - sframe: The sampling frame.
# - sampling_target: a dataframe with the sampling targets by strata.
# - cls: The cluster size.
# Returns:
# - A vector of sampled PSU IDs (one entry per draw).
clustersample <- function(sframe, sampling_target, cls) {
  cluster_sampling(
    sframe,
    cls = cls,
    dist = as.character(sampling_target[["PST_stratum_id"]]),
    target_with_buffer = as.numeric(as.character(
      sampling_target[["target.with.buffer"]]
    ))
  )
}

#' Randomly samples from a sampling frame
#' This function takes a sampling frame, and a buffer size as input.
#' It randomly samples from the sampling frame dataset based on the sampling frame, ensuring that the number of samples does not exceed the population size.
#' The buffer size is used to determine the maximum number of samples to be taken.
#' sframe A sampling frame dataset containing the population information
#' sampling_target A sampling frame containing the strata ID, target with buffer, and population information
#' buf The buffer size to for the samples
#' Returns A vector of randomly selected IDs from the sampling frame dataset
randomsample <- function(sframe, sampling_target, buf) {
  dist <- as.character(sampling_target[["PST_stratum_id"]])
  dbr <- sframe[as.character(sframe$PST_stratum_id) == dist, ]
  tosample <- as.numeric(sampling_target[["target.with.buffer"]])
  pop <- as.numeric(sampling_target[["Population"]])
  if (tosample > pop) {
    target <- ceiling(pop)
  } else {
    target <- ceiling(tosample)
  }
  out <- sample(x = as.character(dbr$PST_id_sample), size = target, replace = FALSE)
  # incProgress(round(1/nrow(sampling_target),2), detail = paste("Sampling", dist))
  return(out)
}

#' stage2rdsample Function
#' This function performs stage 2 random sampling based on given parameters.
#' sframe A data frame containing the sampling frame data.
#' sampling_target A data frame containing the sampling data.
#' buf The buffer size for sampling.
#' returns A vector of randomly selected IDs from the sampling frame data.
stage2rdsample <- function(sframe, sampling_target, buf) {
  dist <- as.character(sampling_target[["PST_stratum_id"]])
  dbr <- sframe[as.character(sframe$PST_stratum_id) == dist, ]
  tosample <- as.numeric(sampling_target[["target.with.buffer"]])
  pop <- as.numeric(sampling_target[["Population"]])
  if (tosample > pop) {
    target <- ceiling(pop)
  } else {
    target <- ceiling(tosample)
  }
  out <- sample(
    x = as.character(dbr$PST_id_sample),
    size = target,
    prob = dbr$PST_prob,
    replace = TRUE
  )
  # incProgress(round(1/nrow(sampling_target),2), detail = paste("Sampling", dist))
  return(out)
}


#' Perform cluster sampling
#'
#' This function performs cluster sampling based on specified parameters.
#' sframe A data frame containing the sampling frame data.
#' cls The (planned) cluster size.
#' dist The stratum ID.
#' target_with_buffer The target sample size, buffer included. Already
#'   DEFF-adjusted by create_targets() for Cluster sampling, so no design
#'   effect is applied here.
#' returns A vector of sampled PSU IDs (one entry per draw). Every PSU must be
#'   able to host a cluster: validate_cluster_size() blocks the other cases.
cluster_sampling <- function(
  sframe,
  cls,
  dist,
  target_with_buffer
) {
  dbr <- sframe[as.character(sframe$PST_stratum_id) == dist, ]
  m <- ceiling(as.numeric(target_with_buffer) / cls)

  sample(as.character(dbr$PST_id_sample), size = m, prob = dbr$PST_prob, replace = TRUE)
}


#' Function to create a sample based on different sampling methods
#'
#' This function takes a sampling frame and input parameters as input and creates a sample based on the specified sampling method.
#' The sampling methods supported are Cluster sampling, Simple random - allocation, and Simple random sampling.
#' The function formats the sampling frame, creates the target sample, and applies the specified sampling method to generate the output.
#' It also calculates various summary statistics related to the sample.
#'
#' sampling_frame The sampling frame data.
#' input The input parameters for the sampling method.
#' return A list containing the sample, summary statistics, and any additional information.
make_sample <- function(sampling_frame, input) {
  # format the sample frame
  sampl_f <- format_sampling_frame(sampling_frame, input)

  # create the target sample.
  target <- create_targets(sampl_f, input)

  output <- c()

  cls <- input$cls
  buf <- input$buf
  ICC <- input$ICC

  if (input$samp_type == "Cluster sampling") {
    output <- apply(
      target,
      1,
      clustersample,
      sframe = sampl_f,
      cls = cls,
      simplify = FALSE
    ) %>%
      unlist
  } else if (input$samp_type == "Simple random - allocation") {
    # simplify = FALSE: when every stratum draws the same number of units
    # (e.g. "Enter sample size" + stratified), apply() would otherwise return
    # a matrix instead of a list, which unlist() keeps 2-D and breaks the
    # merge() below.
    output <- apply(
      target,
      1,
      stage2rdsample,
      sframe = sampl_f,
      buf = buf,
      simplify = FALSE
    ) %>%
      unlist
  } else if (input$samp_type == "Simple random") {
    output <- apply(
      target,
      1,
      randomsample,
      sframe = sampl_f,
      buf = buf,
      simplify = FALSE
    ) %>%
      unlist
  }

  # one row per PSU draw, so repeated draws stay separate visits instead of
  # being collapsed into a single row with a multiplied count
  dbout <- merge(
    data.frame(PST_id_sample = output),
    sampl_f,
    by = "PST_id_sample",
    all.x = T,
    all.y = F
  )

  if (input$samp_type == "Cluster sampling") {
    dbout$PST_survey <- cls
  } else {
    dbout$PST_survey <- 1
  }

  # user-facing sample: one row per selected PSU, with the total number of
  # surveys to run there. Cluster and PPS-allocation draw PSUs with
  # replacement, so dbout can hold the same PSU on several rows; the summary
  # statistics below still use the per-draw dbout.
  dbout_sample <- dbout |>
    dplyr::group_by(PST_id_sample) |>
    dplyr::summarise(
      PST_survey = sum(PST_survey, na.rm = TRUE),
      dplyr::across(-PST_survey, dplyr::first),
      .groups = "drop"
    )

  # create the summary table
  summary_sample <- dbout |>
    dplyr::group_by(PST_stratum_id) |>
    dplyr::summarise(
      Surveys = sum(PST_survey, na.rm = TRUE),
      # PSUs counts draws (PSU selections, with replacement); Unique_PSUs is
      # the number of distinct PSUs a team actually has to visit.
      PSUs = n(),
      Unique_PSUs = dplyr::n_distinct(PST_id_sample),
      NB_Population = max(PST_pop_size_stratum, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      # planned: what was set at design stage, used to compute the target
      # sample size in create_targets()
      Cluster_size_planned = input$cls,
      # realized: measured from the actual draw
      Cluster_size_realized = round(Surveys / PSUs, 2),
      ICC = input$ICC,
      DEFF_planned = 1 + (Cluster_size_planned - 1) * ICC,
      DEFF_realized = 1 + (Cluster_size_realized - 1) * ICC,
      Effective_sample = round(Surveys / DEFF_realized, 0),
      Surveys_buffer = input$buf,
      Confidence_level = input$conf_level,
      Error_margin = input$e_marg,
      Sampling_type = input$samp_type,
      Seed = as.integer(input$seed_value)
    ) |>
    dplyr::left_join(
      target[, c("PST_stratum_id", "target.with.buffer")],
      by = "PST_stratum_id"
    ) |>
    dplyr::relocate(target.with.buffer, .after = NB_Population)

  if (input$samp_type != "Cluster sampling") {
    # "# PSUs to assess" is the draw count; it only differs from "# surveys"
    # when a draw yields more than one survey, i.e. cluster sampling.
    summary_sample[c(
      "PSUs",
      "Cluster_size_realized",
      "Cluster_size_planned",
      "ICC",
      "DEFF_planned",
      "DEFF_realized",
      "Effective_sample"
    )] <- NA
    # plain SRS: no replacement and no PSU concept, so the distinct count
    # is just "# surveys" again.
    if (input$samp_type == "Simple random") summary_sample["Unique_PSUs"] <- NA
  }

  if (input$topup == "Enter sample size") {
    summary_sample[c(
      "ICC",
      "DEFF_planned",
      "DEFF_realized",
      "Effective_sample",
      "Error_margin",
      "Confidence_level",
      "Surveys_buffer"
    )] <- NA
  }

  # rename by name (not position) then drop the columns that are structurally
  # not applicable to this run, so the table only shows relevant statistics.
  summary_sample <- summary_sample |>
    dplyr::rename(
      "Stratification" = "PST_stratum_id",
      "# surveys" = "Surveys",
      "# PSUs to assess" = "PSUs",
      "# Unique PSUs" = "Unique_PSUs",
      "Population" = "NB_Population",
      "Requested Target (with buffer)" = "target.with.buffer",
      "Cluster size set (planned)" = "Cluster_size_planned",
      "Mean Cluster size (realized)" = "Cluster_size_realized",
      "ICC" = "ICC",
      "DEFF (planned)" = "DEFF_planned",
      "DEFF (realized)" = "DEFF_realized",
      "Effective sample size (SRS-equivalent)" = "Effective_sample",
      "% buffer" = "Surveys_buffer",
      "Confidence level" = "Confidence_level",
      "Error margin" = "Error_margin",
      "Sampling type" = "Sampling_type",
      "Seed" = "Seed"
    ) |>
    dplyr::select(dplyr::where(~ !all(is.na(.x))))

  return(list(
    sample = dbout_sample,
    summary_sample = summary_sample
  ))
}
