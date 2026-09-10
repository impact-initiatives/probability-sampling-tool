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

# lift the data size limit
options(shiny.maxRequestSize = 30 * 1024^2)


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

# Calculate the sample size required for a given population proportion
#
# Parameters:
#   population_size: The population size
#   conf_level: The desired level of confidence (between 0 and 1)
#   prop: The estimated population proportion (between 0 and 1)
#   margin_error: The desired margin of error
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
# It adds additional columns to the input data frame, such as id_sampl, strata_id, psu_id, pop_numbers, and proba.
# The id_sampl column is created by concatenating "id_" with the row names of the input data frame.
# If the sampling method is "Stratified", the strata_id column is created by extracting the values from the specified strata column in the input data frame.
# If the sampling method is "Cluster sampling", the psu_id column is created by extracting the values from the specified col_psu column in the input data frame,
# and the pop_numbers column is created by extracting the values from the specified colpop column in the input data frame.
# If the sampling method is "Simple random - allocation" or any other method, the psu_id column is created using the id_sampl column,
# and the pop_numbers column is created with a value of 1 for each row.
# The SumDist column is calculated by summing the pop_numbers column within each strata_id group.
# The proba column is calculated by dividing the pop_numbers column by the SumDist column.
# Any rows with missing values in the proba column are removed.
# If the sampling method is "Cluster sampling", the psu_id column is converted to a factor.
# The resulting data frame is returned.
format_sampling_frame <- function(sframe, input) {
  sframe$id_sampl <- paste0("id_", rownames(sframe))
  if (input$stratified == "Stratified") {
    sframe$strata_id <- sframe[[as.character(input$strata)]]
  } else {
    sframe$strata_id <- rep("all", nrow(sframe))
  }

  if (input$samp_type == "Cluster sampling") {
    sframe$psu_id <- sframe[[as.character(input$col_psu)]]
    sframe$pop_numbers <- sframe[[as.character(input$colpop)]]
  } else if (input$samp_type == "Simple random - allocation") {
    sframe$psu_id <- sframe$id_sampl
    sframe$pop_numbers <- sframe[[as.character(input$colpop)]]
  } else {
    sframe$psu_id <- sframe$id_sampl
    sframe$pop_numbers <- rep(1, nrow(sframe))
  }

  sumdist <- sframe %>%
    dplyr::group_by(strata_id) %>%
    dplyr::summarise(SumDist = sum(pop_numbers, na.rm = T))
  sframe <- merge(sframe, sumdist, by = "strata_id")
  proba <- as.numeric(sframe$pop_numbers) / as.numeric(sframe$SumDist)
  sframe <- cbind(sframe, proba)
  sframe <- sframe[!is.na(sframe$proba), ]

  if (input$samp_type == "Cluster sampling") {
    sframe$psu_id <- as.factor(sframe$psu_id)
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
  return(NULL)
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


# Check that every stratum has at least one PSU large enough for the requested cluster size.
# Returns an error message string if the input is invalid, or NULL if valid.
validate_cluster_size <- function(sampl_f, input) {
  if (input$samp_type != "Cluster sampling") {
    return(NULL)
  }
  eligible <- tapply(
    sampl_f$pop_numbers,
    sampl_f$strata_id,
    function(x) any(x >= input$cls, na.rm = TRUE)
  )
  invalid_strata <- names(eligible)[!eligible]
  if (length(invalid_strata) > 0) {
    return(paste0(
      "Cluster size (",
      input$cls,
      ") exceeds the population of every PSU in stratum(s): ",
      paste(invalid_strata, collapse = ", "),
      ". Reduce the cluster size or review the population column."
    ))
  }
  return(NULL)
}


# Check whether the requested target exceeds the available population in any stratum.
# Returns a character vector of affected stratum names, or NULL if none.
check_target_vs_population <- function(cible) {
  affected <- cible$strata_id[cible$target.with.buffer > cible$Population]
  if (length(affected) > 0) {
    return(as.character(affected))
  }
  return(NULL)
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
# It creates a new column 'strata_id' in the  dataframe based on the input 'strata' value.
# It then groups the dataframe by 'strata_id' and calculates the maximum population value for each group.
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
    dplyr::group_by(strata_id) |>
    dplyr::summarise(
      Population = sum(pop_numbers, na.rm = T)
    ) |>
    dplyr::mutate(
      # base SRS-equivalent target, no design effect - used by the
      # cluster sampling random-sampling fallback (cls=1, DEFF=1)
      target_srs = (if (input$topup == "Enter sample size") {
        input$target
      } else {
        Ssize(
          population_size = Population,
          conf_level = input$conf_level,
          prop = input$pror,
          margin_error = input$e_marg,
          DEFF = 1
        )
      }) |>
        as.numeric(),
      target = (if (input$topup == "Enter sample size") {
        input$target
      } else {
        Ssize(
          population_size = Population,
          conf_level = input$conf_level,
          prop = input$pror,
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


# clustersample function performs cluster sampling based on the given parameters.
# Parameters:
# - sframe: The sampling frame.
# - sampling_target: a dataframe with the sampling targets by strata.
# - cls: The cluster size.
# - buf: The buffer size, used for the random-sampling fallback only (the
#   main draw uses target.with.buffer, already buffer-adjusted).
# - sw_rand: The list of strata IDs that have been switched to random sampling.
# Returns:
# - A list containing the sampled output and the updated sw_rand list.
clustersample <- function(
  sframe,
  sampling_target,
  cls,
  buf,
  sw_rand = c()
) {
  dist <- as.character(sampling_target[["strata_id"]])
  out <- cluster_sampling(
    sframe,
    cls = cls,
    dist = dist,
    target_with_buffer = as.numeric(as.character(
      sampling_target[["target.with.buffer"]]
    ))
  )

  # no PSU big enough for the cluster size: fall back to SRS. Only use of `buf`.
  if (is.null(out)) {
    dbr <- sframe[as.character(sframe$strata_id) == dist, ]
    out <- sample(
      as.character(dbr$id_sampl),
      ceiling(as.numeric(sampling_target[["target_srs"]]) * (1 + buf)),
      prob = dbr$proba,
      replace = TRUE
    )
    sw_rand <- c(sw_rand, dist)
  }
  return(list(output = out, sw_rand = sw_rand))
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
  dist <- as.character(sampling_target[["strata_id"]])
  dbr <- sframe[as.character(sframe$strata_id) == dist, ]
  tosample <- as.numeric(sampling_target[["target.with.buffer"]])
  pop <- as.numeric(sampling_target[["Population"]])
  if (tosample > pop) {
    target <- ceiling(pop)
  } else {
    target <- ceiling(tosample)
  }
  out <- sample(x = as.character(dbr$id_sampl), size = target, replace = FALSE)
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
  dist <- as.character(sampling_target[["strata_id"]])
  dbr <- sframe[as.character(sframe$strata_id) == dist, ]
  tosample <- as.numeric(sampling_target[["target.with.buffer"]])
  pop <- as.numeric(sampling_target[["Population"]])
  if (tosample > pop) {
    target <- ceiling(pop)
  } else {
    target <- ceiling(tosample)
  }
  out <- sample(
    x = as.character(dbr$id_sampl),
    size = target,
    prob = dbr$proba,
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
#' returns A vector of sampled PSU IDs (one entry per draw), or NULL if no
#'   PSU in the stratum is large enough to support the requested cluster size.
cluster_sampling <- function(
  sframe,
  cls,
  dist,
  target_with_buffer
) {
  dbr <- sframe[as.character(sframe$strata_id) == dist, ]
  dbr <- dbr[dbr$pop_numbers >= cls, ]

  if (nrow(dbr) == 0) {
    return(NULL)
  }

  m <- ceiling(as.numeric(target_with_buffer) / cls)

  sample(as.character(dbr$id_sampl), size = m, prob = dbr$proba, replace = TRUE)
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

  sw_rand <- c()
  output <- c()

  cls <- input$cls
  buf <- input$buf
  ICC <- input$ICC

  if (input$samp_type == "Cluster sampling") {
    clsampling <- apply(
      target,
      1,
      clustersample,
      sframe = sampl_f,
      cls = cls,
      # no buffer when the user entered an explicit sample size
      buf = if (input$topup == "Enter sample size") 0 else buf
    )
    output <- lapply(clsampling, function(x) x$output) %>% unlist %>% c
    sw_rand <- lapply(clsampling, function(x) x$sw_rand) %>% unlist %>% c
  } else if (input$samp_type == "Simple random - allocation") {
    # simplify = FALSE: when every stratum draws the same number of units
    # (e.g. "Enter sample size" + stratified), apply() would otherwise return
    # a matrix instead of a list, which unlist() keeps 2-D and breaks the
    # merge() below.
    output <- apply(
      target, 1, stage2rdsample, sframe = sampl_f, buf = buf, simplify = FALSE
    ) %>%
      unlist
  } else if (input$samp_type == "Simple random") {
    output <- apply(
      target, 1, randomsample, sframe = sampl_f, buf = buf, simplify = FALSE
    ) %>%
      unlist
  }

  # one row per PSU draw, so repeated draws stay separate visits instead of
  # being collapsed into a single row with a multiplied count
  dbout <- merge(
    data.frame(output = output),
    sampl_f,
    by.x = "output",
    by.y = "id_sampl",
    all.x = T,
    all.y = F
  )

  if (input$samp_type == "Cluster sampling") {
    dbout$Survey <- ifelse(dbout$strata_id %in% sw_rand, 1, cls)
  } else {
    dbout$Survey <- 1
  }

  names(dbout)[names(dbout) == "output"] <- "id_sampl"

  # user-facing sample: one row per selected PSU, with the total number of
  # surveys to run there. Cluster and PPS-allocation draw PSUs with
  # replacement, so dbout can hold the same PSU on several rows; the summary
  # statistics below still use the per-draw dbout.
  dbout_sample <- dbout |>
    dplyr::group_by(id_sampl) |>
    dplyr::summarise(
      Survey = sum(Survey, na.rm = TRUE),
      dplyr::across(-Survey, dplyr::first),
      .groups = "drop"
    )

  # create the summary table
  summary_sample <- dbout |>
    dplyr::group_by(strata_id) |>
    dplyr::summarise(
      Surveys = sum(Survey, na.rm = TRUE),
      # PSUs counts draws (PSU selections, with replacement); Unique_PSUs is
      # the number of distinct PSUs a team actually has to visit.
      PSUs = n(),
      Unique_PSUs = dplyr::n_distinct(id_sampl),
      NB_Population = max(SumDist, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      # planned: what was set at design stage, used to compute the target
      # sample size in create_targets()
      Cluster_size_planned = input$cls,
      # realized: measured from the actual draw (can differ from the plan,
      # e.g. a stratum falling back to random sampling with cluster size 1)
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
      target[, c("strata_id", "target.with.buffer")],
      by = "strata_id"
    ) |>
    dplyr::relocate(target.with.buffer, .after = NB_Population)

  if (input$samp_type == "Cluster sampling") {
    for (i in 1:nrow(summary_sample)) {
      if (summary_sample$strata_id[i] %in% sw_rand) {
        summary_sample$Cluster_size_realized[i] <- 1
        summary_sample$DEFF_realized[i] <- 1
        summary_sample$Effective_sample[i] <- summary_sample$Surveys[i]
        summary_sample$Sampling_type[
          i
        ] <- "Cluster sampling with size 1 = random sampling"
      }
    }
  }

  if (input$samp_type != "Cluster sampling") {
    # "# PSUs to assess" is the draw count; it only differs from "# surveys"
    # when a draw yields more than one survey, i.e. cluster sampling.
    summary_sample[c(
      "PSUs", "Cluster_size_realized", "Cluster_size_planned",
      "ICC", "DEFF_planned", "DEFF_realized", "Effective_sample"
    )] <- NA
    # plain SRS: no replacement and no PSU concept, so the distinct count
    # is just "# surveys" again.
    if (input$samp_type == "Simple random") summary_sample["Unique_PSUs"] <- NA
  }

  if (input$topup == "Enter sample size") {
    summary_sample[c(
      "ICC", "DEFF_planned", "DEFF_realized", "Effective_sample",
      "Error_margin", "Confidence_level", "Surveys_buffer"
    )] <- NA
  }

  # rename by name (not position) then drop the columns that are structurally
  # not applicable to this run, so the table only shows relevant statistics.
  summary_sample <- summary_sample |>
    dplyr::rename(
      "Stratification" = "strata_id",
      "# surveys" = "Surveys",
      "# PSUs to assess" = "PSUs",
      "# Unique PSUs" = "Unique_PSUs",
      "Population" = "NB_Population",
      "Target (with buffer)" = "target.with.buffer",
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
    summary_sample = summary_sample,
    sw_rand = sw_rand
  ))
}
