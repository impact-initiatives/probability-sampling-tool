# Helpers of the server for the validation messages of the checks.

# Stops with the message when a validate_*() check returned one
require_valid <- function(msg) validate(need(is.null(msg), msg))

# NULL when `expr` succeeds, otherwise its validation message ("" if silent).
validation_message <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    shiny.silent.error = function(e) conditionMessage(e)
  )
}

# Observers must not evaluate an `expr` that can fail without catching it:
# an unexpected error there would end the session.
succeeds <- function(expr) {
  tryCatch(
    {
      expr
      TRUE
    },
    error = function(e) FALSE
  )
}

# Gives up silently (like req(FALSE)) when `expr` stopped on a validation message
quiet <- function(expr) {
  tryCatch(expr, shiny.silent.error = function(e) req(FALSE))
}
