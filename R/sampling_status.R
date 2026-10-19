# What the Sampling tab tells about its state: why "Sample" can or cannot be
# used, and why the tables and the downloads are empty or inert.

#' Say in what state the sampling is and what to do next
#'
#' One state at a time, the first that applies:
#' 1. nothing was applied yet;
#' 2. the setup changed since the last Apply (the sample on screen, if any, was
#'    hidden);
#' 3. the last Apply found a problem;
#' 4. the seed is not valid;
#' 5. the last draw was blocked;
#' 6. a sample is shown, drawn after the last Apply;
#' 7. ready: the setup is applied and "Sample" can be clicked.
#'
#' @param apply_clicked Whether Apply was clicked at least once.
#' @param setup_changed Whether the setup changed since the last Apply.
#' @param applied Whether the last Apply succeeded and nothing changed since.
#' @param seed_message The message about the seed, `NULL` when it is valid.
#' @param draw_failed Whether the last draw was blocked.
#' @param sample_current Whether a sample is on screen: drawn after the last
#'   Apply and not hidden since by a change of the setup.
#' @returns A list with the `id` of the state, its `kind` (`"info"`,
#'   `"warning"`, `"danger"` or `"success"`) and its `text`.
#' @noRd
sampling_status <- function(
  apply_clicked,
  setup_changed,
  applied,
  seed_message,
  draw_failed,
  sample_current
) {
  state <- function(id, kind, text) list(id = id, kind = kind, text = text)
  if (!apply_clicked) {
    return(state(
      "not_applied",
      "info",
      "Nothing is applied yet. Set up the sampling and click Apply in the Sampling frame tab."
    ))
  }
  if (setup_changed) {
    return(state(
      "setup_changed",
      "warning",
      "The setup changed since the last Apply, so any previous sample is hidden. Click Apply again in the Sampling frame tab, then Sample."
    ))
  }
  if (!applied) {
    return(state(
      "apply_failed",
      "danger",
      "The last Apply found a problem. Read the message in the Sampling frame tab, fix it and click Apply again."
    ))
  }
  if (!is.null(seed_message)) {
    return(state(
      "seed_invalid",
      "danger",
      paste(seed_message, "Change it in the Sampling frame tab.")
    ))
  }
  if (draw_failed) {
    return(state(
      "draw_blocked",
      "danger",
      "The last draw was blocked, see the message above the sample summary. Change the setup and click Apply again, or change the seed."
    ))
  }
  if (sample_current) {
    return(state(
      "sampled",
      "success",
      "Sample drawn. Download it with the Export button."
    ))
  }
  state(
    "ready",
    "info",
    "The setup is applied. Click Sample to draw the sample."
  )
}

#' Show the state of the sampling as a note
#'
#' @param status The state, see `sampling_status()`.
#' @returns A note with an icon and the text, as HTML.
#' @noRd
status_note <- function(status) {
  icons <- c(
    info = "circle-info",
    warning = "triangle-exclamation",
    danger = "circle-xmark",
    success = "circle-check"
  )
  div(
    class = paste0("alert py-2 px-3 mt-3 mb-0 alert-", status$kind),
    role = "status",
    style = "font-size: 0.85rem;",
    icon(icons[[status$kind]], `aria-hidden` = "true"),
    " ",
    status$text
  )
}

#' Say why a table is empty
#'
#' Nothing is shown once a sample is on screen.
#'
#' @param status The state, see `sampling_status()`.
#' @param sample_current Whether the sample on screen was drawn after the last
#'   Apply.
#' @returns A muted line, or `NULL`.
#' @noRd
empty_table_note <- function(status, sample_current) {
  if (sample_current) {
    return(NULL)
  }
  div(class = "text-muted", "No sample to show. ", status$text)
}
