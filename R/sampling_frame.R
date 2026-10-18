# The sampling frame: the columns the tool adds to the uploaded file.

#' Add the columns of the tool to the sampling frame
#'
#' - `PST_id_sample`: "id_" followed by the row number;
#' - `PST_stratum_id`: the stratification column, or "all" when not stratified;
#' - `PST_psu_id`: the PSU column for cluster sampling (a factor), the row itself
#'   otherwise;
#' - `PST_pop_size`: the population column for PPS and cluster sampling, 1 for
#'   simple random sampling;
#' - `PST_pop_size_stratum`: the total of `PST_pop_size` in the stratum;
#' - `PST_prob`: `PST_pop_size / PST_pop_size_stratum`, the probability of drawing
#'   the row in its stratum.
#'
#' A PSU of population 0 alone in its stratum gives 0 / 0: cluster sampling keeps
#' it with a probability of 0, so that the cluster size check reports it; the
#' other types drop the row.
#'
#' @param sframe The sampling frame, as read from the uploaded file.
#' @param params The settings of the sampling, see `read_params()`.
#' @returns The frame with the columns of the tool added and put first, see
#'   `FRAME_COLUMN_ORDER`.
#' @noRd
format_sampling_frame <- function(sframe, params) {
  sframe$PST_id_sample <- paste0("id_", rownames(sframe))
  if (params$stratified) {
    sframe$PST_stratum_id <- sframe[[as.character(params$strata_column)]]
  } else {
    sframe$PST_stratum_id <- rep("all", nrow(sframe))
  }

  if (params$sampling_type == SAMPLING_TYPE_CLUSTER) {
    sframe$PST_psu_id <- sframe[[as.character(params$psu_column)]]
    sframe$PST_pop_size <- sframe[[as.character(params$population_column)]]
  } else if (params$sampling_type == SAMPLING_TYPE_PPS) {
    sframe$PST_psu_id <- sframe$PST_id_sample
    sframe$PST_pop_size <- sframe[[as.character(params$population_column)]]
  } else {
    sframe$PST_psu_id <- sframe$PST_id_sample
    sframe$PST_pop_size <- rep(1, nrow(sframe))
  }

  sumdist <- sframe |>
    dplyr::group_by(PST_stratum_id) |>
    dplyr::summarise(PST_pop_size_stratum = sum(PST_pop_size, na.rm = T))
  sframe <- merge(sframe, sumdist, by = "PST_stratum_id")
  PST_prob <- as.numeric(sframe$PST_pop_size) /
    as.numeric(sframe$PST_pop_size_stratum)
  # an empty PSU alone in its stratum gives 0/0: keep it so that the cluster
  # size guard reports it instead of the row vanishing from the frame
  if (params$sampling_type == SAMPLING_TYPE_CLUSTER) {
    PST_prob[is.nan(PST_prob)] <- 0
  }
  sframe <- cbind(sframe, PST_prob)
  sframe <- sframe[!is.na(sframe$PST_prob), ]

  if (params$sampling_type == SAMPLING_TYPE_CLUSTER) {
    sframe$PST_psu_id <- as.factor(sframe$PST_psu_id)
  }
  return(order_columns(sframe, FRAME_COLUMN_ORDER))
}
