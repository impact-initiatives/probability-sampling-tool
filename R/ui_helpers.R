# Building blocks of the interface: help icons, inputs and the references they cite.

# Kish, L. (1965). Survey Sampling. New York: Wiley.
KISH_1965 <- tagList("Kish, ", tags$em("Survey Sampling"), " (1965)")
# Cochran, W. G. (1977). Sampling Techniques, 3rd ed. New York: Wiley.
COCHRAN_1977 <- tagList("Cochran, ", tags$em("Sampling Techniques"), " (1977)")

#' Build the header of a sidebar section
#'
#' It looks like the card headers of the main area.
#'
#' @param text The title.
#' @returns A card header.
#' @noRd
sidebar_header <- function(text) {
  div(
    class = "card bslib-card border-0 rounded-0 bg-transparent",
    card_header(h2(text, class = "h6 mb-0"))
  )
}

#' Build a help icon that opens a tooltip
#'
#' To put inside the label of a field.
#'
#' @param label The label of the field, used in the accessible name of the icon.
#' @param tip The text of the tooltip.
#' @param source A reference shown under the text, or `NULL`.
#' @returns The icon, with its tooltip.
#' @noRd
info_tip <- function(label, tip, source = NULL) {
  tooltip(
    tags$span(
      class = "info-tip",
      tabindex = "0",
      role = "img",
      `aria-label` = paste("About", label),
      icon("circle-info", `aria-hidden` = "true")
    ),
    tip,
    if (!is.null(source)) div(class = "info-tip-source", source)
  )
}

#' Build a numeric input with a help icon in its label
#'
#' The input is named by the text of the label only, not by the icon.
#'
#' @param inputId The id of the input.
#' @param label The label of the input.
#' @param tip The text of the tooltip.
#' @param source A reference shown under the text of the tooltip, or `NULL`.
#' @param ... The other arguments of `numericInput()`.
#' @returns A numeric input.
#' @noRd
numeric_input_with_tip <- function(inputId, label, tip, source = NULL, ...) {
  name_id <- paste0(inputId, "-name")
  htmltools::tagQuery(
    numericInput(
      inputId,
      tagList(
        tags$span(id = name_id, label),
        info_tip(label, tip, source)
      ),
      ...
    )
  )$find("input")$addAttrs("aria-labelledby" = name_id)$allTags()
}

#' Build a select whose list opens over the page
#'
#' Inside the setup sidebar, which scrolls its content, a list that opens near
#' the bottom of the card is cut off.
#'
#' @param inputId The id of the input.
#' @param label The label of the input.
#' @param choices The choices of the list, or `NULL` to fill it later.
#' @param ... The other arguments of `selectizeInput()`.
#' @returns A select input.
#' @noRd
select_input_overlay <- function(inputId, label, choices = NULL, ...) {
  selectizeInput(
    inputId,
    label,
    choices = choices,
    ...,
    options = list(dropdownParent = "body")
  )
}

#' Build a JavaScript condition: an input has a value
#'
#' For the `condition` of a `conditionalPanel()`.
#'
#' @param id The id of the input.
#' @param value The value to compare with.
#' @returns A string.
#' @noRd
js_equals <- function(id, value) sprintf("input.%s == '%s'", id, value)
#' Build a JavaScript condition: an input does not have a value
#'
#' For the `condition` of a `conditionalPanel()`.
#'
#' @param id The id of the input.
#' @param value The value to compare with.
#' @returns A string.
#' @noRd
js_differs <- function(id, value) sprintf("input.%s != '%s'", id, value)

#' Build the sidebar of a tab
#'
#' Open on desktop and resizable.
#'
#' @param id The id of the sidebar.
#' @param width The width in pixels.
#' @param ... The content of the sidebar.
#' @returns A sidebar.
#' @noRd
tab_sidebar <- function(id, width, ...) {
  sidebar(
    id = id,
    width = width,
    open = "desktop",
    resizable = TRUE,
    fillable = FALSE,
    gap = 0,
    padding = 0,
    ...
  )
}

#' Build a card with a title around a table
#'
#' It can also hold the message that goes with the table.
#'
#' @param title The title of the card.
#' @param ... The content of the card.
#' @returns A card.
#' @noRd
table_card <- function(title, ...) {
  card(
    card_header(h2(title, class = "h6 mb-0")),
    card_body(fillable = FALSE, ...)
  )
}
