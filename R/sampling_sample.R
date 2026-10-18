# The sample and its summary.

# Function to create a sample based on different sampling methods
#
# This function takes a sampling frame and input parameters as input and creates a sample based on the specified sampling method.
# The sampling types supported are two-stage PPS cluster sampling with replacement, PPS sampling with replacement and simple random sampling.
# The function formats the sampling frame, creates the target sample, and applies the specified sampling method to generate the output.
# It also calculates various summary statistics related to the sample.
#
# sampling_frame The sampling frame data.
# params The settings of the sampling, as returned by read_params().
# return A list containing the sample, summary statistics, and any additional information.
make_sample <- function(sampling_frame, params) {
  # format the sample frame
  sampl_f <- format_sampling_frame(sampling_frame, params)

  # create the target sample.
  target <- create_targets(sampl_f, params)

  cls <- params$cluster_size

  if (params$sampling_type == SAMPLING_TYPE_CLUSTER) {
    output <- apply(
      target,
      1,
      pps_cluster_sampling,
      sframe = sampl_f,
      cls = cls,
      simplify = FALSE
    ) |>
      unlist()
  } else if (params$sampling_type == SAMPLING_TYPE_PPS) {
    # simplify = FALSE: when every stratum draws the same number of units
    # (e.g. "Enter sample size" + stratified), apply() would otherwise return
    # a matrix instead of a list, which unlist() keeps 2-D and breaks the
    # merge() below.
    output <- apply(
      target,
      1,
      pps_sampling,
      sframe = sampl_f,
      simplify = FALSE
    ) |>
      unlist()
  } else if (params$sampling_type == SAMPLING_TYPE_SIMPLE) {
    output <- apply(
      target,
      1,
      simple_random_sampling,
      sframe = sampl_f,
      simplify = FALSE
    ) |>
      unlist()
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

  if (params$sampling_type == SAMPLING_TYPE_CLUSTER) {
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
    ) |>
    order_columns(SAMPLE_COLUMN_ORDER)

  # create the summary table
  summary_sample <- dbout |>
    dplyr::group_by(PST_stratum_id) |>
    dplyr::summarise(
      Surveys = sum(PST_survey, na.rm = TRUE),
      # PSUs counts draws (PSU selections, with replacement); Unique_PSUs is
      # the number of distinct PSUs a team actually has to visit.
      PSUs = dplyr::n(),
      Unique_PSUs = dplyr::n_distinct(PST_id_sample),
      NB_Population = max(PST_pop_size_stratum, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      # every draw is an independent cluster of `cls` surveys, also when the
      # same PSU is drawn several times: the cluster size, and so the design
      # effect, are set by the design, not measured on the draw. DEFF uses the
      # ICC entered by the user.
      Cluster_size = params$cluster_size,
      ICC = params$icc,
      DEFF = 1 + (Cluster_size - 1) * ICC,
      Effective_sample = round(Surveys / DEFF, 0),
      Surveys_buffer = params$buffer,
      Confidence_level = params$confidence_level,
      Error_margin = params$error_margin,
      Sampling_type = params$sampling_type,
      Seed = as.integer(params$seed)
    ) |>
    dplyr::left_join(
      target[, c("PST_stratum_id", "target.with.buffer")],
      by = "PST_stratum_id"
    ) |>
    dplyr::relocate(target.with.buffer, .after = NB_Population)

  if (params$sampling_type != SAMPLING_TYPE_CLUSTER) {
    # "Clusters drawn" is the draw count; it only differs from "Surveys"
    # when a draw yields more than one survey, i.e. cluster sampling.
    summary_sample[c(
      "PSUs",
      "Cluster_size",
      "ICC",
      "DEFF",
      "Effective_sample"
    )] <- NA
    # plain SRS: no replacement and no PSU concept, so the distinct count
    # is just "Surveys" again.
    if (params$sampling_type == SAMPLING_TYPE_SIMPLE) {
      summary_sample["Unique_PSUs"] <- NA
    }
  }

  if (params$sampling_mode == SAMPLING_MODE_ENTER_SIZE) {
    summary_sample[c(
      "ICC",
      "DEFF",
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
      "Stratum" = "PST_stratum_id",
      "Surveys" = "Surveys",
      "Clusters drawn" = "PSUs",
      "Distinct PSUs" = "Unique_PSUs",
      "Population" = "NB_Population",
      "Requested Target (with buffer)" = "target.with.buffer",
      "Cluster size" = "Cluster_size",
      "ICC" = "ICC",
      "DEFF" = "DEFF",
      "Effective sample size" = "Effective_sample",
      "Buffer" = "Surveys_buffer",
      "Confidence level" = "Confidence_level",
      "Error margin" = "Error_margin",
      "Sampling type" = "Sampling_type",
      "Seed" = "Seed"
    ) |>
    dplyr::select(dplyr::where(~ !all(is.na(.x)))) |>
    order_columns(SUMMARY_COLUMN_ORDER)

  return(list(
    sample = dbout_sample,
    summary_sample = summary_sample
  ))
}
