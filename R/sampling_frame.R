# The sampling frame: the columns the tool adds to the uploaded file.

# create the sampling frame
# This function creates a sampling frame based on the input parameters.
# It adds additional columns to the input data frame, such as PST_id_sample, PST_stratum_id, PST_psu_id, PST_pop_size, and PST_prob.
# The PST_id_sample column is created by concatenating "id_" with the row names of the input data frame.
# If the sampling method is "Stratified", the PST_stratum_id column is created by extracting the values from the specified strata column in the input data frame.
# If the sampling method is "Two-stage PPS cluster sampling with replacement", the PST_psu_id column is created by extracting the values from the specified col_psu column in the input data frame,
# and the PST_pop_size column is created by extracting the values from the specified colpop column in the input data frame.
# If the sampling method is "PPS sampling with replacement" or any other method, the PST_psu_id column is created using the PST_id_sample column,
# and the PST_pop_size column is created with a value of 1 for each row.
# The PST_pop_size_stratum column is calculated by summing the PST_pop_size column within each PST_stratum_id group.
# The PST_prob column is calculated by dividing the PST_pop_size column by the PST_pop_size_stratum column.
# Any rows with missing values in the PST_prob column are removed.
# If the sampling method is "Two-stage PPS cluster sampling with replacement", the PST_psu_id column is converted to a factor.
# The resulting data frame is returned.
format_sampling_frame <- function(sframe, input) {
  sframe$PST_id_sample <- paste0("id_", rownames(sframe))
  if (input$stratified == STRATIFIED) {
    sframe$PST_stratum_id <- sframe[[as.character(input$strata)]]
  } else {
    sframe$PST_stratum_id <- rep("all", nrow(sframe))
  }

  if (input$samp_type == SAMPLING_TYPE_CLUSTER) {
    sframe$PST_psu_id <- sframe[[as.character(input$col_psu)]]
    sframe$PST_pop_size <- sframe[[as.character(input$colpop)]]
  } else if (input$samp_type == SAMPLING_TYPE_PPS) {
    sframe$PST_psu_id <- sframe$PST_id_sample
    sframe$PST_pop_size <- sframe[[as.character(input$colpop)]]
  } else {
    sframe$PST_psu_id <- sframe$PST_id_sample
    sframe$PST_pop_size <- rep(1, nrow(sframe))
  }

  sumdist <- sframe %>%
    dplyr::group_by(PST_stratum_id) %>%
    dplyr::summarise(PST_pop_size_stratum = sum(PST_pop_size, na.rm = T))
  sframe <- merge(sframe, sumdist, by = "PST_stratum_id")
  PST_prob <- as.numeric(sframe$PST_pop_size) /
    as.numeric(sframe$PST_pop_size_stratum)
  # an empty PSU alone in its stratum gives 0/0: keep it so that the cluster
  # size guard reports it instead of the row vanishing from the frame
  if (input$samp_type == SAMPLING_TYPE_CLUSTER) {
    PST_prob[is.nan(PST_prob)] <- 0
  }
  sframe <- cbind(sframe, PST_prob)
  sframe <- sframe[!is.na(sframe$PST_prob), ]

  if (input$samp_type == SAMPLING_TYPE_CLUSTER) {
    sframe$PST_psu_id <- as.factor(sframe$PST_psu_id)
  }
  return(order_columns(sframe, FRAME_COLUMN_ORDER))
}
