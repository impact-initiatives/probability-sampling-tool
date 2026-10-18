# The draw of one stratum, for each sampling type.

# pps_cluster_sampling draws the PSUs of one stratum (two-stage PPS cluster sampling). The guard in
# validate_cluster_size() must have passed: there is no fallback.
# Parameters:
# - sframe: The sampling frame.
# - sampling_target: a dataframe with the sampling targets by strata.
# - cls: The cluster size.
# Returns:
# - A vector of sampled PSU IDs (one entry per draw).
pps_cluster_sampling <- function(sframe, sampling_target, cls) {
  draw_pps_clusters(
    sframe,
    cls = cls,
    dist = as.character(sampling_target[["PST_stratum_id"]]),
    target_with_buffer = as.numeric(as.character(
      sampling_target[["target.with.buffer"]]
    ))
  )
}

#' Simple random sampling, for one stratum
#'
#' Draws units with equal probability and without replacement, so a unit is
#' drawn at most once. The frame lists the units to survey (households,
#' individuals). The number of units is the target with buffer of the stratum,
#' capped to its population.
#'
#' sframe The sampling frame, as returned by format_sampling_frame().
#' sampling_target One row of create_targets(): the stratum, its population and
#'   its target with buffer.
#' buf Not used: the buffer is already in the target with buffer. Kept so that
#'   all the draw functions are called the same way.
#' returns A vector of the sampled ids (PST_id_sample), each one once.
simple_random_sampling <- function(sframe, sampling_target, buf) {
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
    replace = FALSE
  )
  # incProgress(round(1/nrow(sampling_target),2), detail = paste("Sampling", dist))
  return(out)
}

#' PPS sampling with replacement, for one stratum
#'
#' Draws the units of a stratum one at a time, with replacement, with a
#' probability proportional to their size (PST_prob, from the population
#' column): a large unit can be drawn several times and receives one survey per
#' draw. The frame lists areas with their population, not the individuals to
#' survey (see simple_random_sampling() for that). The number of draws is the
#' target with buffer of the stratum, capped to its population. It is the same
#' draw as the first stage of pps_cluster_sampling(), with one survey per draw.
#'
#' sframe The sampling frame, as returned by format_sampling_frame().
#' sampling_target One row of create_targets(): the stratum, its population and
#'   its target with buffer.
#' buf Not used: the buffer is already in the target with buffer. Kept so that
#'   all the draw functions are called the same way.
#' returns A vector of the sampled ids (PST_id_sample), one entry per draw, so
#'   an id can appear several times.
pps_sampling <- function(sframe, sampling_target, buf) {
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


#' First stage of two-stage PPS cluster sampling with replacement
#'
#' Draws the PSUs of one stratum with replacement, with a probability
#' proportional to their population. Each draw stands for one cluster of `cls`
#' surveys: the households of the cluster are selected afterwards, at data
#' collection.
#' sframe A data frame containing the sampling frame data.
#' cls The (planned) cluster size.
#' dist The stratum ID.
#' target_with_buffer The target sample size, buffer included. Already
#'   DEFF-adjusted by create_targets() for cluster sampling, so no design
#'   effect is applied here.
#' returns A vector of sampled PSU IDs (one entry per draw). Every PSU must be
#'   able to host a cluster: validate_cluster_size() blocks the other cases.
draw_pps_clusters <- function(
  sframe,
  cls,
  dist,
  target_with_buffer
) {
  dbr <- sframe[as.character(sframe$PST_stratum_id) == dist, ]
  m <- ceiling(as.numeric(target_with_buffer) / cls)

  sample(
    as.character(dbr$PST_id_sample),
    size = m,
    prob = dbr$PST_prob,
    replace = TRUE
  )
}
