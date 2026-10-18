# The checks of the app, read from guide/checks.yml: every message the user can
# get from a check is written there once. The code builds its messages with
# check_message() and the guide builds its tables from the same file.
#
# The file is read when the app starts, so that a mistake in it stops the app
# at once instead of the first time a message is needed.
CHECKS <- yaml::read_yaml("guide/checks.yml")

# Replaces each {name} of `template` by the matching value of the list `values`
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

# The message of the check `id`, with its {name} parts filled in by `...`
check_message <- function(id, ...) {
  check <- CHECKS$checks[[id]]
  if (is.null(check)) {
    stop("Unknown check: ", id, call. = FALSE)
  }
  fill_template(check$message, list(...))
}

# The title of a check (its `title` in the file), for the warnings that have one
check_title <- function(id) {
  title <- CHECKS$checks[[id]]$title
  if (is.null(title)) {
    stop("No title for the check: ", id, call. = FALSE)
  }
  title
}

# A sentence that the code adds to a message (the `texts` of the file)
check_text <- function(id) {
  text <- CHECKS$texts[[id]]
  if (is.null(text)) {
    stop("Unknown text: ", id, call. = FALSE)
  }
  text
}
