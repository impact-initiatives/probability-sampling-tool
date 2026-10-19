# The page around the tabs: its theme and its header.

#' Build the Bootstrap theme of the app
#'
#'
#'
#' @returns A `bs_theme`.
#' @noRd
app_theme <- function() {
  theme <- bs_theme(
    version = 5,
    # WCAG AA: white text on primary/danger/success is above 4.5:1
    primary = "#1A56A0",
    secondary = "#5C6670",
    success = "#146C43",
    danger = "#B42318",
    "body-bg" = "#F5F7FA",
    # form control borders need 3:1 against the background (WCAG 1.4.11)
    "input-border-color" = "#8A939C",
    "card-cap-bg" = "#EEF3FA",
    "card-cap-color" = "#12355B",
    base_font = font_collection(
      "system-ui",
      "-apple-system",
      "Segoe UI",
      "Roboto",
      "Helvetica Neue",
      "Arial",
      "sans-serif"
    ),
    # the Bootstrap 3 base size, to keep the setup card on one screen
    "font-size-base" = "0.875rem",
    # the Bootstrap 3 paragraph spacing, which keeps the Guide compact
    "paragraph-margin-bottom" = "0.7rem",
    "h1-font-size" = "2rem",
    "h2-font-size" = "1.4rem"
  )
}

#' Build the header of the page
#'
#' The scripts and styles loaded with the page: the busy indicators, the check
#' of the size of an uploaded file and the style sheet.
#'
#' @returns A tag list.
#' @noRd
app_header <- function() {
  header <- tagList(
    shinyjs::useShinyjs(),
    # Spinner on the outputs being recalculated and a pulse while the app is busy
    useBusyIndicators(),
    # Shiny drops a file above the size limit silently: report its name to the
    # server (input$rejected_file_name) and do not upload it
    tags$script(HTML(sprintf(
      "document.addEventListener('change', function(e) {
				if (e.target.id !== 'frame_file') return;
				var big = Array.from(e.target.files).find(function(f) { return f.size > %d; });
				Shiny.setInputValue('rejected_file_name', big ? big.name : null, {priority: 'event'});
				if (big) { e.stopImmediatePropagation(); }
			}, true);",
      as.integer(MAX_UPLOAD_MB * 1024^2)
    ))),
    # The box of the file input shows the name of the file the user chose. A
    # browser can put the name back after a reload although the new session has no
    # file: clear it when the page connects, a new session never has a file yet.
    tags$script(HTML(
      "$(document).on('shiny:connected', function() {
        $('#frame_file').closest('.input-group').find('input[type=text]').val('');
      });"
    )),
    # Cache-bust: without a version query string, browsers keep serving a
    # previously-cached app.css after it changes on disk.
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0("app.css?v=", as.integer(file.mtime("www/app.css")))
    )
  )
}
