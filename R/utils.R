# Small helpers used all over the app.

#' Get the current time as text
#'
#' Used in the names of the downloaded files, for example `20260101-120000`.
#'
#' @returns A string, the date and time down to the second.
#' @noRd
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

#' Run an expression under a given random seed
#'
#' The previous state of the random number generator is restored afterwards, so
#' the seed does not affect any other randomness in the session.
#'
#' @param seed The seed, a whole number.
#' @param expr The expression to evaluate.
#' @returns The value of `expr`.
#' @noRd
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

#' Put the first columns of a table first
#'
#' The columns of `first` come first, in this order, then the others in their own
#' order. A column of `first` that is not in `data` is ignored.
#'
#' @param data A data frame or a data.table.
#' @param first The names of the columns to put first, in order.
#' @returns The table with its columns reordered.
#' @noRd
order_columns <- function(data, first) {
  first <- intersect(first, names(data))
  # select(), not `[`: the frame read by fread() is a data.table
  dplyr::select(data, dplyr::all_of(c(first, setdiff(names(data), first))))
}

#' Test for a single finite number
#'
#'
#'
#' @param x Any value.
#' @returns `TRUE` for a single finite number; `FALSE` for `NULL`, `NA`, `NaN`, `Inf` and
#'   vectors.
#' @noRd
is_number <- function(x) {
  length(x) == 1 && is.numeric(x) && is.finite(x)
}

#' Show a few ids
#'
#' For example `a, b, c, ... +4 more`.
#'
#' @param ids The ids to show.
#' @param n The most ids to show.
#' @returns A string.
#' @noRd
preview_ids <- function(ids, n = 5) {
  ids <- as.character(ids)
  if (length(ids) <= n) {
    return(paste(ids, collapse = ", "))
  }
  paste0(paste(ids[1:n], collapse = ", "), ", ... +", length(ids) - n, " more")
}
