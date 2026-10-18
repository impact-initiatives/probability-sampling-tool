# The guide: its Markdown texts, and the cards and steps that show them.

# Text of the guide: Markdown files in guide/ (and NEWS.md), so that it can be
# reviewed and corrected without touching the code. {{max_upload_mb}} is filled in.
guide_markdown <- function(text) {
  text <- gsub("{{max_upload_mb}}", max_upload_mb, text, fixed = TRUE)
  # markdown::mark() turns $...$ into \(...\) for MathJax, unlike shiny::markdown()
  div(
    class = "guide-text",
    HTML(markdown::mark(text = text, format = "html", template = FALSE))
  )
}

guide_text <- function(file) {
  guide_markdown(paste(readLines(file, warn = FALSE), collapse = "\n"))
}

# A Markdown file shown as one card per second level heading, in a grid. What
# comes before the first heading is shown above the cards.
guide_sections <- function(file, width = "30rem") {
  lines <- readLines(file, warn = FALSE)
  starts <- grep("^## ", lines)
  intro <- if (starts[1] > 1) lines[seq_len(starts[1] - 1)] else character()
  ends <- c(starts[-1] - 1, length(lines))
  cards <- lapply(seq_along(starts), function(i) {
    body <- lines[(starts[i] + 1):ends[i]]
    card(
      class = "guide-card h-100",
      fill = FALSE,
      card_header(h3(
        sub("^## ", "", lines[starts[i]]),
        class = "h6 fw-semibold mb-0"
      )),
      card_body(fillable = FALSE, guide_markdown(paste(body, collapse = "\n")))
    )
  })
  tagList(
    if (length(intro) > 0 && any(nzchar(intro))) {
      guide_markdown(paste(intro, collapse = "\n"))
    },
    do.call(
      layout_column_wrap,
      c(list(width = width, fill = FALSE, heights_equal = "row"), cards)
    )
  )
}

# A titled block of the guide, around a Markdown file
guide_card <- function(title, file) {
  card(
    class = "guide-card h-100",
    fill = FALSE,
    card_header(h3(title, class = "h6 fw-semibold mb-0")),
    card_body(fillable = FALSE, guide_text(file))
  )
}

# The checks of the app, by group, closed until opened
guide_checks <- function() {
  card(
    class = "guide-card",
    fill = FALSE,
    card_header(h3("What the tool checks", class = "h6 fw-semibold mb-0")),
    card_body(
      fillable = FALSE,
      accordion(
        id = "checks_accordion",
        open = FALSE,
        multiple = TRUE,
        accordion_panel(
          "The file",
          value = "file",
          guide_text("guide/checks-file.md")
        ),
        accordion_panel(
          "The columns",
          value = "columns",
          guide_text("guide/checks-columns.md")
        ),
        accordion_panel(
          "The parameters",
          value = "parameters",
          guide_text("guide/checks-parameters.md")
        ),
        accordion_panel(
          "The cluster design",
          value = "cluster",
          guide_text("guide/checks-cluster.md")
        ),
        accordion_panel(
          "The draw",
          value = "draw",
          guide_text("guide/checks-draw.md")
        )
      )
    )
  )
}

# A step of the "How it works" row of the guide; the link sits at the bottom
guide_step <- function(number, title, text, link = NULL) {
  card(
    class = "guide-step",
    card_body(
      class = "d-flex flex-column",
      div(
        class = "d-flex align-items-center gap-2 mb-1",
        span(class = "guide-step-number", number),
        h3(title, class = "h6 fw-semibold mb-0")
      ),
      p(text, class = "small mb-2"),
      if (!is.null(link)) div(class = "mt-auto small", link)
    )
  )
}

# A card of the "Three sampling types" row of the guide; the link sits at the bottom
guide_type_card <- function(name, before, one_row, needs, button_id) {
  card(
    class = "guide-type",
    card_header(
      div(
        h3(name, class = "h6 fw-semibold mb-1"),
        span(class = "badge text-bg-light fw-normal", paste("before:", before))
      )
    ),
    card_body(
      class = "d-flex flex-column",
      p(strong("One row is "), one_row, class = "small mb-1"),
      p(strong("Needs "), needs, class = "small mb-2"),
      div(
        class = "mt-auto small",
        actionLink(button_id, tagList("Details ", icon("arrow-right")))
      )
    )
  )
}

# The landing of the guide
guide_start <- function() {
  tagList(
    div(
      class = "guide-hero",
      h1("Draw a probability sample from your sampling frame"),
      p(
        class = "lead",
        "Load your CSV, choose a sampling type and set the parameters. The tool returns the units to survey and the number of surveys to run in each."
      ),
      actionButton(
        "guide_to_app",
        tagList("Start sampling ", icon("arrow-right")),
        class = "btn-primary"
      )
    ),
    h2("How it works", class = "h6 fw-semibold mt-3"),
    layout_column_wrap(
      width = "14rem",
      fill = FALSE,
      guide_step(
        1,
        "Prepare the frame",
        "One CSV, one row per unit of your frame.",
        actionLink("guide_to_data", "What the frame must contain")
      ),
      guide_step(
        2,
        "Set up",
        "Mode, type of sampling, columns and parameters in the Sampling frame tab.",
        actionLink("guide_to_parameters", "The parameters")
      ),
      guide_step(
        3,
        "Apply",
        "Computes the target per stratum and checks the frame and the parameters."
      ),
      guide_step(
        4,
        "Sample and export",
        "Draws the sample in the Sampling tab, then download the Sample and its summary.",
        actionLink("guide_to_results", "Reading the results")
      )
    ),
    h2("Three sampling types", class = "h6 fw-semibold mt-3"),
    layout_column_wrap(
      width = "17rem",
      fill = FALSE,
      guide_type_card(
        "Simple random sampling",
        "Simple random",
        "a unit to survey (household, person).",
        "no population column.",
        "guide_open_simple_random"
      ),
      guide_type_card(
        "PPS sampling with replacement",
        "Simple random - allocation",
        "a PSU (an area) with its population.",
        "a population column.",
        "guide_open_pps"
      ),
      guide_type_card(
        "Two-stage PPS cluster sampling with replacement",
        "Cluster sampling",
        "a PSU with its population.",
        "a cluster column and a population column.",
        "guide_open_cluster"
      )
    ),
    div(
      class = "alert alert-info mt-3 mb-0 py-2 d-flex justify-content-between align-items-center",
      span(
        icon("circle-info"),
        strong(" Coming from the previous version? "),
        "The sampling types and the columns of the results have new names."
      ),
      actionLink("guide_to_news", "What's new")
    )
  )
}
