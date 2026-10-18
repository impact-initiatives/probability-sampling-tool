# The settings of the sampling, as one plain list.

# The fields of the list, named like the inputs of the interface they come from
PARAM_FIELDS <- c(
  "topup",
  "samp_type",
  "stratified",
  "strata",
  "col_psu",
  "colpop",
  "conf_level",
  "e_marg",
  "buf",
  "target",
  "cls",
  "ICC",
  "seed_value"
)

# Reads the settings of the sampling from the inputs of the interface. The
# checks and the calculation take this list, not the Shiny inputs: they do not
# depend on the interface.
read_params <- function(input) {
  stats::setNames(
    lapply(PARAM_FIELDS, function(field) input[[field]]),
    PARAM_FIELDS
  )
}
