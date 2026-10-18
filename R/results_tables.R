# The results as shown to the user.

# Warning shown when the requested sample size exceeds the population of some
# strata (`affected`). `drawn` is TRUE once the sample has been drawn.
exceeds_population_modal <- function(affected, drawn = FALSE) {
  modalDialog(
    title = tagList(
      icon("triangle-exclamation"),
      "Sample size exceeds population"
    ),
    div(
      class = "alert alert-warning",
      paste0(
        "The requested sample size exceeds the available population in stratum(s): ",
        paste(affected, collapse = ", "),
        ". The sample ",
        if (drawn) "was" else "will be",
        " capped to the available population there."
      )
    ),
    easyClose = TRUE,
    footer = modalButton("OK")
  )
}
