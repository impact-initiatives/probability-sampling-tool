# Building blocks of the interface: help icons, inputs and the references they cite.

# Kish, L. (1965). Survey Sampling. New York: Wiley.
KISH_1965 <- tagList("Kish, ", tags$em("Survey Sampling"), " (1965)")
# Cochran, W. G. (1977). Sampling Techniques, 3rd ed. New York: Wiley.
COCHRAN_1977 <- tagList("Cochran, ", tags$em("Sampling Techniques"), " (1977)")

# Header of a sidebar section, identical to the card headers of the main area
sidebar_header <- function(text) {
  div(
    class = "card bslib-card border-0 rounded-0 bg-transparent",
    card_header(h2(text, class = "h6 mb-0"))
  )
}

# Help icon opening a tooltip, to put inside a field label.
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

# numericInput() whose label carries a help icon with a tooltip. The input is
# named by the label text only, not by the icon.
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

# selectInput() whose list opens over the page. Inside the setup sidebar, which
# scrolls its content, a list opening near the bottom of the card is cut off.
select_input_overlay <- function(inputId, label, choices = NULL, ...) {
  selectizeInput(
    inputId,
    label,
    choices = choices,
    ...,
    options = list(dropdownParent = "body")
  )
}
