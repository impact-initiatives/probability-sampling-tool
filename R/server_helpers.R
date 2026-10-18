# Helpers of the server for the validation messages of the checks.

#' Stop with the message of a check
#'
#'
#'
#' @param msg The output of a `validate_*()` check: a message, or `NULL`.
#' @returns Nothing when `msg` is `NULL`; otherwise it stops the calling reactive with
#'   `msg`, as a Shiny validation error.
#' @noRd
require_valid <- function(msg) validate(need(is.null(msg), msg))

#' Get the validation message of an expression
#'
#'
#'
#' @param expr An expression that may stop on a validation message.
#' @returns `NULL` when `expr` succeeds, otherwise its message (`""` if it has none).
#' @noRd
validation_message <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    shiny.silent.error = function(e) conditionMessage(e)
  )
}

#' Test that an expression does not fail
#'
#' Observers must not evaluate an expression that can fail without catching it:
#' an unexpected error there would end the session.
#'
#' @param expr An expression.
#' @returns `TRUE` when `expr` succeeds, `FALSE` when it raises an error.
#' @noRd
succeeds <- function(expr) {
  tryCatch(
    {
      expr
      TRUE
    },
    error = function(e) FALSE
  )
}

#' Give up silently when an expression stops on a validation message
#'
#'
#'
#' @param expr An expression that may stop on a validation message.
#' @returns The value of `expr`; if it stopped on a validation message, it stops silently
#'   like `req(FALSE)`.
#' @noRd
quiet <- function(expr) {
  tryCatch(expr, shiny.silent.error = function(e) req(FALSE))
}
