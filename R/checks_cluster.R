# Checks of the cluster design: at Apply, and on the drawn sample.

# Cluster sampling guard, run before any draw: it depends on the data and the
# parameters only, never on the seed. It looks for
# - PSUs smaller than the cluster size: they cannot host a cluster;
# - strata that cannot host the clusters their target requires: they need
#   ceiling(target / cls) clusters but their PSUs have room for
#   sum(floor(population / cls)).
# `target` is the output of create_targets().
# Returns one row per problem (a PSU row has a `psu`, a stratum row has none),
# or NULL when there is no problem.
cluster_size_issues <- function(sampl_f, target, input) {
  if (input$samp_type != "Two-stage PPS cluster sampling with replacement") {
    return(NULL)
  }
  cls <- input$cls
  strata <- as.character(sampl_f$PST_stratum_id)
  pop <- sampl_f$PST_pop_size
  psu_rows <- NULL
  stratum_rows <- NULL

  too_small <- !is.na(pop) & pop < cls
  if (any(too_small)) {
    psu_rows <- data.frame(
      issue = "PSU too small for the cluster size",
      stratum = strata[too_small],
      psu = as.character(sampl_f$PST_psu_id[too_small]),
      n_psus = NA_integer_,
      population = pop[too_small],
      cluster_size = cls,
      target_with_buffer = NA_real_,
      max_surveys = NA_real_,
      shortfall = NA_real_
    )
  }

  capacity <- tapply(floor(pop / cls), strata, sum, na.rm = TRUE)
  target_by_stratum <- setNames(
    target$target.with.buffer,
    as.character(target$PST_stratum_id)
  )
  needed <- ceiling(target_by_stratum[names(capacity)] / cls)
  short <- names(capacity)[capacity < needed]
  if (length(short) > 0) {
    stratum_rows <- data.frame(
      issue = "Stratum cannot reach its target with this cluster size",
      stratum = short,
      psu = NA_character_,
      n_psus = as.integer(table(strata)[short]),
      population = setNames(
        target$Population,
        as.character(target$PST_stratum_id)
      )[
        short
      ],
      cluster_size = cls,
      target_with_buffer = target_by_stratum[short],
      max_surveys = capacity[short] * cls,
      shortfall = target_by_stratum[short] - capacity[short] * cls
    )
    stratum_rows <- stratum_rows[order(-stratum_rows$shortfall), ]
  }
  out <- rbind(psu_rows, stratum_rows)
  if (!is.null(out)) {
    rownames(out) <- NULL
  }
  out
}


# Error message for the output of cluster_size_issues(), NULL when there is none.
# Each problem has its own paragraph and only a few examples: the full list is
# downloadable.
cluster_size_message <- function(issues, cls) {
  if (is.null(issues)) {
    return(NULL)
  }
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  blocks <- character()

  psus <- issues[!is.na(issues$psu), ]
  if (nrow(psus) > 0) {
    blocks <- c(
      blocks,
      paste0(
        "PSUs smaller than the cluster size (",
        cls,
        "): ",
        nrow(psus),
        " PSU(s) cannot host a cluster - ",
        preview_ids(
          paste0(
            psus$psu,
            " in '",
            psus$stratum,
            "' (population ",
            fmt(psus$population),
            ")"
          ),
          n = 3
        ),
        ".\nReduce the cluster size or filter these PSUs out of the sampling frame."
      )
    )
  }

  strata <- issues[is.na(issues$psu), ]
  if (nrow(strata) > 0) {
    blocks <- c(
      blocks,
      paste0(
        "Insufficient stratum capacity: ",
        nrow(strata),
        " stratum(s) cannot reach their target with clusters of ",
        cls,
        " surveys. Largest shortfalls - ",
        preview_ids(
          paste0(
            "'",
            strata$stratum,
            "' (target ",
            fmt(strata$target_with_buffer),
            ", maximum ",
            fmt(strata$max_surveys),
            ")"
          ),
          n = 3
        ),
        ".\nReduce the cluster size or the target sample size, or merge PSUs."
      )
    )
  }
  paste(blocks, collapse = "\n\n")
}

# Cluster sampling draws PSUs with replacement: a PSU drawn several times is
# assigned cluster size x draws surveys, which can exceed its population. Unlike
# the guard at Apply this depends on the seed, so it is checked on the drawn
# sample (the output of make_sample()$sample).
# Returns one row per PSU concerned, or NULL when there is none.
psu_overflow_issues <- function(sample, input) {
  if (input$samp_type != "Two-stage PPS cluster sampling with replacement") {
    return(NULL)
  }
  over <- which(sample$PST_survey > sample$PST_pop_size)
  if (length(over) == 0) {
    return(NULL)
  }
  s <- sample[over, ]
  out <- data.frame(
    issue = "PSU drawn more often than its population allows",
    stratum = as.character(s$PST_stratum_id),
    psu = as.character(s$PST_psu_id),
    times_drawn = s$PST_survey / input$cls,
    cluster_size = input$cls,
    surveys_assigned = s$PST_survey,
    population = s$PST_pop_size,
    excess = s$PST_survey - s$PST_pop_size
  )
  out[order(-out$excess), ]
}


# Error message for the output of psu_overflow_issues(), NULL when there is none.
psu_overflow_message <- function(issues) {
  if (is.null(issues)) {
    return(NULL)
  }
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  paste0(
    "Draw blocked: ",
    nrow(issues),
    " PSU(s) were selected several times and would need more surveys than their population - ",
    preview_ids(
      paste0(
        "'",
        issues$psu,
        "' in '",
        issues$stratum,
        "' (drawn ",
        issues$times_drawn,
        " times, ",
        fmt(issues$surveys_assigned),
        " surveys for a population of ",
        fmt(issues$population),
        ")"
      ),
      n = 3
    ),
    ".\nReduce the cluster size or the target sample size, or merge PSUs."
  )
}
