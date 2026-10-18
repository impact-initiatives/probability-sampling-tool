# Checks of the sampling parameters, run at Apply.

#' Check the numeric settings of the sampling
#'
#' Looks for values that make the sample size computation or the draw fail. The
#' `min` and `max` of the numeric inputs mirror these rules, but Shiny does not
#' enforce them: it sends whatever is typed. Only the settings used by the
#' selected mode and sampling type are checked.
#'
#' @param params The settings of the sampling, see `read_params()`.
#' @returns An error message for the first invalid setting, `NULL` when all are valid.
#' @noRd
validate_parameters <- function(params) {
  strictly_between_0_and_1 <- function(x) is_number(x) && x > 0 && x < 1

  if (params$sampling_mode == SAMPLING_MODE_ENTER_SIZE) {
    target <- params$target_size
    if (!is_number(target) || target <= 0 || target != floor(target)) {
      return(check_message("target_size"))
    }
  } else {
    if (!strictly_between_0_and_1(params$confidence_level)) {
      return(check_message("confidence_level"))
    }
    if (!strictly_between_0_and_1(params$error_margin)) {
      return(check_message("error_margin"))
    }
    if (!is_number(params$buffer) || params$buffer < 0) {
      return(check_message("buffer"))
    }
  }

  if (params$sampling_type == SAMPLING_TYPE_CLUSTER) {
    cls <- params$cluster_size
    if (!is_number(cls) || cls < 2 || cls != floor(cls)) {
      return(check_message("cluster_size"))
    }
    if (!is_number(params$icc) || params$icc < 0 || params$icc > 1) {
      return(check_message("icc"))
    }
  }
  return(NULL)
}

#' Check the seed
#'
#' The seed must be a single finite whole number within the range of
#' `set.seed()`. This guards against a cleared field (`NA`) or a decimal or
#' out-of-range value that would make `set.seed()` fail and break the sampling.
#'
#' @param seed The value of the seed input.
#' @returns An error message when the seed is invalid, `NULL` otherwise.
#' @noRd
validate_seed <- function(seed) {
  if (
    length(seed) != 1 ||
      is.na(seed) ||
      !is.finite(seed) ||
      seed != floor(seed) ||
      abs(seed) > .Machine$integer.max
  ) {
    return(check_message("seed"))
  }
  return(NULL)
}
