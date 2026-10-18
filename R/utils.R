# Small helpers used all over the app.

# humanTime function returns the current time in a specific format.
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# Runs `expr` under a given RNG seed, restoring the previous RNG state
# afterwards so the seed doesn't affect any other randomness in the session.
run_with_seed <- function(seed, expr) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  })
  set.seed(seed)
  expr
}

# Puts the columns of `first` first, in this order, then the others in their
# own order. A column of `first` that is not in `data` is ignored.
order_columns <- function(data, first) {
  first <- intersect(first, names(data))
  # select(), not `[`: the frame read by fread() is a data.table
  dplyr::select(data, dplyr::all_of(c(first, setdiff(names(data), first))))
}

# TRUE for a single finite number (FALSE for NULL, NA, NaN, Inf or vectors).
is_number <- function(x) {
  length(x) == 1 && is.numeric(x) && is.finite(x)
}

# Shows at most `n` ids, e.g. "a, b, c, ... +4 more".
preview_ids <- function(ids, n = 5) {
  ids <- as.character(ids)
  if (length(ids) <= n) {
    return(paste(ids, collapse = ", "))
  }
  paste0(paste(ids[1:n], collapse = ", "), ", ... +", length(ids) - n, " more")
}
