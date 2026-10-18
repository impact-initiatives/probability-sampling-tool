# The target sample size of each stratum.

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
  DEFF <- if (input$samp_type == SAMPLING_TYPE_CLUSTER) {
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
      target = (if (input$topup == SAMPLING_MODE_ENTER_SIZE) {
        input$target
      } else {
        Ssize(
          population_size = Population,
          conf_level = input$conf_level,
          prop = FIXED_PROPORTION,
          margin_error = input$e_marg,
          DEFF = DEFF
        )
      }) |>
        as.numeric(),
      target.with.buffer = if (input$topup == SAMPLING_MODE_ENTER_SIZE) {
        target
      } else {
        as.numeric(ceiling(target * (1 + input$buf)))
      }
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
