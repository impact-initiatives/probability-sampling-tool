# The sample and its summary.

#' Draw the sample and build its summary
#'
#' Formats the sampling frame, computes the targets, applies the draw of the
#' sampling type and builds the summary of the sample. The three types are
#' two-stage PPS cluster sampling with replacement, PPS sampling with replacement
#' and simple random sampling.
#'
#' @param sampling_frame The sampling frame, as read from the uploaded file.
#' @param params The settings of the sampling, see `read_params()`.
#' @returns A list with `sample` (one row per selected unit, with the number of surveys to
#'   run there) and `summary_sample` (one row per stratum).
#' @noRd
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
      surveys = sum(PST_survey, na.rm = TRUE),
      # clusters_drawn counts draws (PSU selections, with replacement);
      # distinct_psus is the number of distinct PSUs a team actually has to visit.
      clusters_drawn = dplyr::n(),
      distinct_psus = dplyr::n_distinct(PST_id_sample),
      population = max(PST_pop_size_stratum, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      # every draw is an independent cluster of `cluster_size` surveys, also when
      # the same PSU is drawn several times: the cluster size, and so the design
      # effect, are set by the design, not measured on the draw. The design
      # effect uses the ICC entered by the user.
      cluster_size = params$cluster_size,
      icc = params$icc,
      deff = 1 + (cluster_size - 1) * icc,
      effective_sample_size = round(surveys / deff, 0),
      buffer = params$buffer,
      confidence_level = params$confidence_level,
      error_margin = params$error_margin,
      sampling_type = params$sampling_type,
      seed = as.integer(params$seed)
    ) |>
    dplyr::left_join(
      target[, c("PST_stratum_id", "target_with_buffer")],
      by = "PST_stratum_id"
    ) |>
    dplyr::relocate(target_with_buffer, .after = population)

  if (params$sampling_type != SAMPLING_TYPE_CLUSTER) {
    # "Clusters drawn" is the draw count; it only differs from "Surveys"
    # when a draw yields more than one survey, i.e. cluster sampling.
    summary_sample[c(
      "clusters_drawn",
      "cluster_size",
      "icc",
      "deff",
      "effective_sample_size"
    )] <- NA
    # plain SRS: no replacement and no PSU concept, so the distinct count
    # is just "Surveys" again.
    if (params$sampling_type == SAMPLING_TYPE_SIMPLE) {
      summary_sample["distinct_psus"] <- NA
    }
  }

  if (params$sampling_mode == SAMPLING_MODE_ENTER_SIZE) {
    summary_sample[c(
      "icc",
      "deff",
      "effective_sample_size",
      "error_margin",
      "confidence_level",
      "buffer"
    )] <- NA
  }

  # give the columns their labels, then drop the columns that are structurally
  # not applicable to this run, so the table only shows relevant statistics.
  summary_sample <- summary_sample |>
    dplyr::rename(dplyr::all_of(SUMMARY_LABELS)) |>
    dplyr::select(dplyr::where(~ !all(is.na(.x)))) |>
    order_columns(SUMMARY_COLUMN_ORDER)

  return(list(
    sample = dbout_sample,
    summary_sample = summary_sample
  ))
}
