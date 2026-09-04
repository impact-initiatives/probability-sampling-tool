# Debug script for cluster sampling methodology.
# Runs format_sampling_frame(), create_targets(), cluster_sampling(),
# clustersample() and make_sample() directly, without going through the Shiny
# app, so each step's intermediate output can be inspected/stepped through.
#
# Run from the repo root: Rscript debug/debug_cluster_sampling.R
# Or open in RStudio and run line by line / with breakpoints inside global.R.

source("global.R")

df <- read.csv("debug/debug_sampling_frame.csv", stringsAsFactors = FALSE)

# debug_sampling_frame.csv layout:
#   region      - stratification column   ("North" = 4 PSUs, "South" = 12 PSUs)
#   psu_name    - PSU (cluster) column, one row per PSU
#   population  - PSU size (measure of size used for PPS weighting)
# "North" has few PSUs on purpose: it is small enough to exhaust all PSUs
# before reaching the DESS-adjusted target, triggering the random-sampling
# fallback in clustersample() (global.R:149-164). "South" has enough PSUs to
# go through cluster_sampling() normally, for comparison.

# Mock Shiny's `input` reactive list with a plain named list.
# Field names/values below must exactly match global.R's input$... usages
# and the choices defined in ui.R (topup, samp_type, stratified selectInputs).
input <- list(
    # --- sampling design ---
    samp_type = "Cluster sampling", # "Simple random" | "Simple random - allocation" | "Cluster sampling"
    stratified = "Stratified", # "Not stratified" | "Stratified"
    strata = "region", # column name used for strata_id when stratified
    col_psu = "psu_name", # column name used for psu_id (Cluster sampling only)
    colpop = "population", # column name used for pop_numbers

    # --- sample size mode ---
    topup = "Sample size based on population", # "Enter sample size" | "Sample size based on population"
    target = 20, # only used if topup == "Enter sample size"
    conf_level = 0.95, # only used if topup == "Sample size based on population"
    pror = 0.5,
    e_marg = 0.05,

    # --- cluster sampling parameters ---
    cls = 5, # cluster size (households surveyed per selected PSU)
    buf = 0.05, # buffer added on top of the target sample size
    ICC = 0.06 # intra-cluster correlation coefficient, used to compute DESS
)

cat("\n=== STEP 1: format_sampling_frame() ===\n")
sframe <- format_sampling_frame(df, input)
print(sframe)

cat("\n=== STEP 2: create_targets() ===\n")
targets <- create_targets(sframe, input)
print(targets)

cat(
    "\n=== STEP 3: cluster_sampling() breakdown, run sub-step by sub-step ===\n"
)
# Each block below is a single statement lifted directly from cluster_sampling()
# (global.R:230-280). Run them one at a time (e.g. select + Cmd/Ctrl+Enter in
# RStudio) to inspect every intermediate variable in your environment.

# 3.1 - parameters (mirrors the function's arguments, global.R:230-238)
north_target <- targets[targets$strata_id == "North", ]
# derived the same way clustersample() does it (global.R:139), not hardcoded
dist <- as.character(north_target[["strata_id"]])
target <- north_target$target
cls <- input$cls
buf <- input$buf
ICC <- input$ICC
mode <- "notforced"
cat("dist =", dist, "| target =", target, "| cls =", cls, "\n")

# 3.2 - eligible PSU pool: subset to this stratum, then drop PSUs too small
#       for the requested cluster size (global.R:240-241)
dbr <- sframe[as.character(sframe$strata_id) == dist, ]
dbr <- dbr[dbr$pop_numbers >= cls, ]
print(dbr[, c("id_sampl", "pop_numbers", "proba")])

# 3.3 - initial PPS draw of PSUs, with replacement (global.R:242-247)
out <- sample(
    as.character(dbr$id_sampl),
    ceiling(as.numeric(target * (1 + buf)) / cls),
    prob = dbr$proba,
    replace = TRUE
)
cat("initial out:", paste(out, collapse = ", "), "\n")

# --- one iteration of the while loop (global.R:251-278) ---
# Re-run 3.4a-3.4d as a block, repeatedly, until the printed message tells
# you to stop. Each run advances the loop by exactly one iteration and
# mutates `out` in place, same as the real while loop would.

# 3.4a - realised cluster sizes so far, and the DESS-adjusted stopping target
#        (global.R:252-255)
d <- as.data.frame(table(out))[, 2]
ms <- sum(d) / nrow(as.data.frame(d))
DESS <- 1 + (ms * cls - 1) * ICC
targ <- DESS * (target * (1 + buf)) / cls
cat(sprintf(
    "sum(d)=%d vs targ=%.2f (ms=%.2f, DESS=%.2f)\n",
    sum(d),
    targ,
    ms,
    DESS
))

# 3.4b - stop condition: target reached (global.R:257-260)
# If TRUE is printed below, STOP HERE: `out` is the final result, do not
# run 3.4c/3.4d.
cat("target reached? ", sum(d) >= targ, "\n")

# 3.4c - not reached yet: draw one more PSU and append it (global.R:266-269)
out <- c(
    out,
    sample(as.character(dbr$id_sampl), 1, prob = dbr$proba, replace = TRUE)
)
cat("out is now:", paste(out, collapse = ", "), "\n")

# 3.4d - has every eligible PSU already been hit at least once?
#        (global.R:270-276). If TRUE below (in "notforced" mode), the
#        stratum is exhausted: cluster_sampling() returns NULL and
#        clustersample() (STEP 4) would trigger its random-sampling fallback.
rd_check <- all(unique(dbr$id_sampl) %in% unique(out))
cat("all PSUs exhausted?", rd_check, "\n")
if (rd_check && mode == "notforced") {
    out <- NULL
    cat("-> exhausted: out set to NULL, stop iterating.\n")
} else {
    cat("-> not exhausted yet: go back and re-run 3.4a-3.4d.\n")
}

cat(
    "\n=== STEP 3bis: same call via the real cluster_sampling() function, for comparison ===\n"
)
north_out <- cluster_sampling(
    sframe,
    cls = input$cls,
    buf = input$buf,
    ICC = input$ICC,
    dist = dist,
    target = north_target$target
)
cat("Result (NULL means all PSUs were exhausted -> fallback would trigger):\n")
print(north_out)

cat("\n=== STEP 4: clustersample() breakdown, run sub-step by sub-step ===\n")
# Picks up where STEP 3 left off: `out` is either a vector (target reached)
# or NULL (exhausted). Each block mirrors clustersample() (global.R:130-165).

# 4.1 - did cluster_sampling() give up? (global.R:149)
cat("is.null(out)?", is.null(north_out), "\n")

# 4.2 - fallback: draw directly at "cluster size = 1", i.e. one interview
#       per hit instead of `cls` per hit (global.R:150-156)
sw_rand <- c()
if (is.null(north_out)) {
    dbr_fallback <- sframe[as.character(sframe$strata_id) == dist, ]
    north_out <- sample(
        as.character(dbr_fallback$id_sampl),
        ceiling(as.numeric(north_target[["target"]]) * (1 + buf + 0.1)),
        prob = dbr_fallback$proba,
        replace = TRUE
    )
    # 4.3 - flag this stratum as switched to random sampling (global.R:163)
    sw_rand <- c(sw_rand, dist)
}
cat("sw_rand:", sw_rand, "\n")
print(table(north_out))

cat(
    "\n=== STEP 5: clustersample() on the South stratum (no fallback expected) ===\n"
)
south_target <- targets[targets$strata_id == "South", ]
south_result <- clustersample(
    sframe,
    sampling_target = south_target,
    cls = input$cls,
    buf = input$buf,
    ICC = input$ICC
)
cat("sw_rand:", south_result$sw_rand, "\n")
print(table(south_result$output))

cat("\n=== STEP 6: make_sample() end-to-end ===\n")
result <- make_sample(df, input)
cat("\n--- sample (per selected PSU) ---\n")
print(result$sample)
cat("\n--- summary_sample ---\n")
print(result$summary_sample)
cat("\n--- sw_rand ---\n")
print(result$sw_rand)
