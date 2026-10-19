# The checks of the app, read from guide/checks.yml: every message the user can
# get from a check is written there once. The code builds its messages with
# check_message() and the guide builds its tables from the same file.
#
# The file is read when the app starts, so that a mistake in it stops the app
# at once instead of the first time a message is needed.
CHECKS <- yaml::read_yaml("guide/checks.yml")

#' Fill the names of a message template
#'
#' Each `{name}` of the template is replaced by the matching value.
#'
#' @param template A text with `{name}` parts.
#' @param values A named list: the value of each name.
#' @returns The text with the names filled in; a name without a value is left as it is.
#' @noRd
fill_template <- function(template, values) {
  for (name in names(values)) {
    template <- gsub(
      paste0("{", name, "}"),
      as.character(values[[name]]),
      template,
      fixed = TRUE
    )
  }
  template
}

#' Get the message of a check
#'
#' The message is written in `guide/checks.yml`; the code only fills in its parts.
#'
#' @param id The identifier of the check, a key of `checks` in the file.
#' @param ... The values of the `{name}` parts of the message.
#' @returns The message, as a string. An unknown `id` is an error.
#' @noRd
check_message <- function(id, ...) {
  check <- CHECKS$checks[[id]]
  if (is.null(check)) {
    stop("Unknown check: ", id, call. = FALSE)
  }
  fill_template(check$message, list(...))
}

#' Get the title of a check
#'
#' Only the checks that warn have a title: it is the title of their window.
#'
#' @param id The identifier of the check.
#' @returns The title, as a string. A check without a title is an error.
#' @noRd
check_title <- function(id) {
  title <- CHECKS$checks[[id]]$title
  if (is.null(title)) {
    stop("No title for the check: ", id, call. = FALSE)
  }
  title
}

#' Get a sentence that the code adds to a message
#'
#' These sentences are the `texts` of `guide/checks.yml`; the guide does not show
#' them.
#'
#' @param id The identifier of the text.
#' @returns The sentence, as a string. An unknown `id` is an error.
#' @noRd
check_text <- function(id) {
  text <- CHECKS$texts[[id]]
  if (is.null(text)) {
    stop("Unknown text: ", id, call. = FALSE)
  }
  text
}
