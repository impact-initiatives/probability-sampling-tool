# Checks of the sampling parameters, run at Apply.

# Check the sampling parameters that make the sample size computation or the
# draw fail when they are mathematically invalid. The min/max of the numeric
# inputs mirror these rules, but Shiny does not enforce them: it sends whatever
# is typed. Only the parameters
# used by the selected mode and sampling type are checked.
# Returns an error message string if one is invalid, or NULL if all are valid.
validate_parameters <- function(input) {
  strictly_between_0_and_1 <- function(x) is_number(x) && x > 0 && x < 1

  if (input$topup == SAMPLING_MODE_ENTER_SIZE) {
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

  if (input$samp_type == SAMPLING_TYPE_CLUSTER) {
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
