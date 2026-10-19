# The draw of one stratum, for each sampling type.

#' Draw the PSUs of one stratum, for cluster sampling
#'
#' Two-stage PPS cluster sampling: this is the first stage, see
#' `draw_pps_clusters()`. The check `cluster_size_issues()`, run at Apply, must have
#' passed: there is no fallback.
#'
#' @param sframe The sampling frame, as returned by `format_sampling_frame()`.
#' @param sampling_target One row of `create_targets()`: the stratum, its population and its target
#'   with buffer.
#' @param cls The cluster size.
#' @returns The sampled PSU ids, one entry per draw.
#' @noRd
pps_cluster_sampling <- function(sframe, sampling_target, cls) {
  draw_pps_clusters(
    sframe,
    cls = cls,
    dist = as.character(sampling_target[["PST_stratum_id"]]),
    target_with_buffer = as.numeric(as.character(
      sampling_target[["target_with_buffer"]]
    ))
  )
}

#' Draw a simple random sample of one stratum
#'
#' Units are drawn with equal probability and without replacement, so a unit is
#' drawn at most once. The frame lists the units to survey (households,
#' individuals). The number of units is the target with buffer of the stratum,
#' capped to its population.
#'
#' @param sframe The sampling frame, as returned by `format_sampling_frame()`.
#' @param sampling_target One row of `create_targets()`: the stratum, its population and its target
#'   with buffer.
#' @returns The sampled ids (`PST_id_sample`), each one once.
#' @noRd
simple_random_sampling <- function(sframe, sampling_target) {
  dist <- as.character(sampling_target[["PST_stratum_id"]])
  dbr <- sframe[as.character(sframe$PST_stratum_id) == dist, ]
  tosample <- as.numeric(sampling_target[["target_with_buffer"]])
  pop <- as.numeric(sampling_target[["population"]])
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
  return(out)
}

#' Draw a PPS sample with replacement of one stratum
#'
#' The units of a stratum are drawn one at a time, with replacement, with a
#' probability proportional to their size (`PST_prob`, from the population
#' column): a large unit can be drawn several times and receives one survey per
#' draw. The frame lists areas with their population, not the individuals to
#' survey (see `simple_random_sampling()` for that). The number of draws is the
#' target with buffer of the stratum, capped to its population. It is the same
#' draw as the first stage of `pps_cluster_sampling()`, with one survey per draw.
#'
#' @param sframe The sampling frame, as returned by `format_sampling_frame()`.
#' @param sampling_target One row of `create_targets()`: the stratum, its population and its target
#'   with buffer.
#' @returns The sampled ids (`PST_id_sample`), one entry per draw, so an id can appear
#'   several times.
#' @noRd
pps_sampling <- function(sframe, sampling_target) {
  dist <- as.character(sampling_target[["PST_stratum_id"]])
  dbr <- sframe[as.character(sframe$PST_stratum_id) == dist, ]
  tosample <- as.numeric(sampling_target[["target_with_buffer"]])
  pop <- as.numeric(sampling_target[["population"]])
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
  return(out)
}


#' Draw the first stage of two-stage PPS cluster sampling
#'
#' The PSUs of one stratum are drawn with replacement, with a probability
#' proportional to their population. Each draw stands for one cluster of `cls`
#' surveys: the households of the cluster are selected afterwards, at data
#' collection.
#'
#' @param sframe The sampling frame, as returned by `format_sampling_frame()`.
#' @param cls The planned cluster size.
#' @param dist The stratum id.
#' @param target_with_buffer The target sample size of the stratum, buffer included, as computed by
#'   `create_targets()`. The design effect, when it changes the size (only in the
#'   mode based on the population), is already in it: it is not applied here.
#' @returns The sampled PSU ids, one entry per draw. Every PSU must be able to host a
#'   cluster: `cluster_size_issues()` blocks the other cases.
#' @noRd
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
