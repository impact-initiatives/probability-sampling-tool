# The target sample size of each stratum.

#' Compute the sample size for a proportion
#'
#' Cochran's formula for a proportion (Cochran, 1977, Sampling Techniques, 3rd
#' ed.) with the finite population correction:
#'
#'     n0 = z^2 * p * (1 - p) / e^2         sample size for an infinite population
#'     n  = n0 / (1 + (n0 - 1) / N)         finite population correction
#'        = z^2 * N * p * (1 - p) / (e^2 * (N - 1) + z^2 * p * (1 - p))
#'
#' z is the two-sided normal quantile of the confidence level: z^2 is also
#' `qchisq(confidence_level, df = 1)`. The result is then multiplied by the design
#' effect.
#'
#' @param population_size The population size (N).
#' @param confidence_level The level of confidence, between 0 and 1.
#' @param proportion The estimated proportion of the population (p), between 0 and 1.
#' @param error_margin The margin of error (e).
#' @param deff The design effect that inflates the sample size; 1 for none.
#' @returns The sample size that gives the confidence level and the margin of error,
#'   rounded up.
#' @noRd
Ssize <- function(
  population_size,
  confidence_level,
  proportion,
  error_margin,
  deff = 1
) {
  z <- qnorm(1 - (1 - confidence_level) / 2) # two-sided z-score for confidence level
  n0 <- (z^2 * proportion * (1 - proportion)) / error_margin^2 # infinite population approximation
  n <- n0 / (1 + (n0 - 1) / population_size) # finite population correction
  ceiling(n * deff)
}

#' Compute the target sample size of each stratum
#'
#' One row per stratum:
#' - `population`: the total of `PST_pop_size`;
#' - `target`: in the mode "Enter sample size", the number typed by the user (the
#'   same for every stratum); in the mode based on the population, the size
#'   computed by `Ssize()`, multiplied by the design effect for cluster sampling;
#' - `target_with_buffer`: the target again in the mode "Enter sample size",
#'   otherwise `target * (1 + buffer)`, rounded up.
#'
#' @param sframe The sampling frame, as returned by `format_sampling_frame()`.
#' @param params The settings of the sampling, see `read_params()`.
#' @returns A table with the columns `PST_stratum_id`, `population`, `target` and
#'   `target_with_buffer`.
#' @noRd
create_targets <- function(sframe, params) {
  # The design effect, 1 + (cluster size - 1) * ICC, applies to cluster sampling
  # only (it is 1 for the other types), and it only changes the sample size when
  # this is computed from the population. In the mode "Enter sample size" the
  # typed target is used as it is: the ICC does not change it, it only gives the
  # design effect and the effective sample size of the summary.
  deff <- if (params$sampling_type == SAMPLING_TYPE_CLUSTER) {
    1 + (params$cluster_size - 1) * params$icc
  } else {
    1
  }

  sframe |>
    dplyr::group_by(PST_stratum_id) |>
    dplyr::summarise(
      population = sum(PST_pop_size, na.rm = T)
    ) |>
    dplyr::mutate(
      target = (if (params$sampling_mode == SAMPLING_MODE_ENTER_SIZE) {
        params$target_size
      } else {
        Ssize(
          population_size = population,
          confidence_level = params$confidence_level,
          proportion = FIXED_PROPORTION,
          error_margin = params$error_margin,
          deff = deff
        )
      }) |>
        as.numeric(),
      target_with_buffer = if (
        params$sampling_mode == SAMPLING_MODE_ENTER_SIZE
      ) {
        target
      } else {
        as.numeric(ceiling(target * (1 + params$buffer)))
      }
    )
}

#' Find the strata where the target exceeds the population
#'
#'
#'
#' @param targets The targets, as returned by `create_targets()`.
#' @returns The names of the strata concerned, `NULL` when there is none.
#' @noRd
check_target_vs_population <- function(targets) {
  affected <- targets$PST_stratum_id[
    targets$target_with_buffer > targets$population
  ]
  if (length(affected) > 0) {
    return(as.character(affected))
  }
  return(NULL)
}

#' Give the Target sampling table its labels
#'
#' The values are what was requested: in a random sampling type the draw is capped
#' to the population of the stratum, so they can be larger than the population next
#' to them.
#'
#' @param targets The targets, as returned by `create_targets()`.
#' @returns The table with the labels of `TARGET_LABELS` as column names.
#' @noRd
label_target_table <- function(targets) {
  dplyr::rename(targets, dplyr::all_of(TARGET_LABELS))
}
