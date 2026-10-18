# The guide: its Markdown texts, and the cards and steps that show them.

# The label of a column of the tables, from the name the code gives it
column_label <- function(key) {
  label <- names(COLUMN_LABELS)[match(key, COLUMN_LABELS)]
  if (is.na(label)) {
    stop("No label for the column: ", key, call. = FALSE)
  }
  label
}

# What a {{...}} of a guide text stands for: the value of a constant
# ({{MAX_UPLOAD_MB}}), the label of a column ({{label:clusters_drawn}}) or the
# title of a check ({{check_title:exceeds_population}}).
guide_value <- function(name) {
  if (startsWith(name, "label:")) {
    return(column_label(sub("^label:", "", name)))
  }
  if (startsWith(name, "check_title:")) {
    return(check_title(sub("^check_title:", "", name)))
  }
  if (!exists(name)) {
    stop("Unknown {{", name, "}} in a guide text", call. = FALSE)
  }
  as.character(get(name))
}

# Replaces every {{...}} of `text` by its value, so that a name or a limit that
# the guide mentions is the one of the code, not a copy of it
fill_guide_placeholders <- function(text) {
  found <- unique(unlist(regmatches(
    text,
    gregexpr("\\{\\{[^}]+\\}\\}", text)
  )))
  for (token in found) {
    value <- guide_value(sub("^\\{\\{(.*)\\}\\}$", "\\1", token))
    text <- gsub(token, value, text, fixed = TRUE)
  }
  text
}

# Text of the guide: Markdown files in guide/ (and NEWS.md). The {{...}} of a
# text are filled in by fill_guide_placeholders(); NEWS.md has none, as the
# release notes tell what the names were at the time.
guide_markdown <- function(text) {
  text <- fill_guide_placeholders(text)
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
guide_checks <- function(ns) {
  panels <- lapply(names(CHECKS$groups), function(id) {
    accordion_panel(
      CHECKS$groups[[id]]$title,
      value = id,
      guide_markdown(guide_checks_markdown(id))
    )
  })
  card(
    class = "guide-card",
    fill = FALSE,
    card_header(h3("What the tool checks", class = "h6 fw-semibold mb-0")),
    card_body(
      fillable = FALSE,
      do.call(
        accordion,
        c(
          list(id = ns("checks_accordion"), open = FALSE, multiple = TRUE),
          panels
        )
      )
    )
  )
}

# The Markdown of a group of checks: when it is checked, the table of its
# messages and what to do, then its note. A check that only warns is not in the
# table: the note of the group tells about it.
guide_checks_markdown <- function(group_id) {
  group <- CHECKS$groups[[group_id]]
  blocks <- Filter(
    function(check) {
      check$group == group_id && !identical(check$kind, "warning")
    },
    CHECKS$checks
  )
  cell <- function(text) gsub("|", "\\|", text, fixed = TRUE)
  rows <- vapply(
    blocks,
    function(check) {
      shown <- if (is.null(check$guide_message)) {
        first_line <- strsplit(check$message, "\n", fixed = TRUE)[[1]][1]
        gsub("\\{[a-z_]+\\}", "\u2026", first_line)
      } else {
        check$guide_message
      }
      applies <- if (is.null(check$applies_to)) {
        ""
      } else {
        paste0(" (", check$applies_to, ")")
      }
      paste0("| *", cell(shown), "*", applies, " | ", cell(check$fix), " |")
    },
    character(1)
  )
  paste(
    c(
      paste0("*", group$when, "*"),
      "",
      "| Message | What to do |",
      "|---|---|",
      unname(rows),
      if (!is.null(group$note)) c("", group$note)
    ),
    collapse = "\n"
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
guide_start <- function(ns) {
  tagList(
    div(
      class = "guide-hero",
      h1("Draw a probability sample from your sampling frame"),
      p(
        class = "lead",
        "Load your CSV, choose a sampling type and set the parameters. The tool returns the units to survey and the number of surveys to run in each."
      ),
      actionButton(
        ns("to_app"),
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
        actionLink(ns("to_data"), "What the frame must contain")
      ),
      guide_step(
        2,
        "Set up",
        "Mode, type of sampling, columns and parameters in the Sampling frame tab.",
        actionLink(ns("to_parameters"), "The parameters")
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
        actionLink(ns("to_results"), "Reading the results")
      )
    ),
    h2("Three sampling types", class = "h6 fw-semibold mt-3"),
    layout_column_wrap(
      width = "17rem",
      fill = FALSE,
      guide_type_card(
        SAMPLING_TYPE_SIMPLE,
        "Simple random",
        "a unit to survey (household, person).",
        "no population column.",
        ns("open_simple_random")
      ),
      guide_type_card(
        SAMPLING_TYPE_PPS,
        "Simple random - allocation",
        "a PSU (an area) with its population.",
        "a population column.",
        ns("open_pps")
      ),
      guide_type_card(
        SAMPLING_TYPE_CLUSTER,
        "Cluster sampling",
        "a PSU with its population.",
        "a cluster column and a population column.",
        ns("open_cluster")
      )
    ),
    div(
      class = "alert alert-info mt-3 mb-0 py-2 d-flex justify-content-between align-items-center",
      span(
        icon("circle-info"),
        strong(" Coming from the previous version? "),
        "The sampling types and the columns of the results have new names."
      ),
      actionLink(ns("to_news"), "What's new")
    )
  )
}

# The guide, a module: the "Guide" tab with one pill per section. Its links, to
# its own sections and to the app, are handled by guide_server().
guide_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Guide",
    withMathJax(
      div(
        class = "guide",
        navset_pill(
          id = ns("nav"),
          nav_panel("Start", value = "start", guide_start(ns)),
          nav_panel("Sampling types", value = "types", guide_types(ns)),
          nav_panel(
            "Your data",
            value = "data",
            layout_columns(
              col_widths = c(5, 7),
              fill = FALSE,
              guide_card("The file", "guide/data-file.md"),
              guide_card(
                "What each type expects",
                "guide/data-types.md"
              )
            ),
            guide_checks(ns)
          ),
          nav_panel(
            "Parameters",
            value = "parameters",
            guide_card("Mode of sampling", "guide/parameters-mode.md"),
            guide_card("Parameters", "guide/parameters-list.md"),
            guide_card(
              "Which parameters apply",
              "guide/parameters-applies.md"
            )
          ),
          nav_panel(
            "Results",
            value = "results",
            guide_card("The tables", "guide/results-tables.md"),
            guide_card(
              "Columns added by the tool",
              "guide/results-columns.md"
            ),
            guide_card("Sample summary", "guide/results-summary.md")
          ),
          nav_panel(
            "What's new",
            value = "news",
            guide_sections("NEWS.md", "27rem")
          ),
          nav_panel(
            "Methods",
            value = "methods",
            guide_sections("guide/methods.md", "30rem")
          )
        )
      )
    )
  )
}

# The three sampling types, one panel each, closed until opened
guide_types <- function(ns) {
  accordion(
    id = ns("types_accordion"),
    open = FALSE,
    multiple = FALSE,
    guide_type_panel(
      SAMPLING_TYPE_SIMPLE,
      "Simple random",
      "simple_random",
      "guide/type-simple-random.md"
    ),
    guide_type_panel(
      SAMPLING_TYPE_PPS,
      "Simple random - allocation",
      "pps",
      "guide/type-pps.md"
    ),
    guide_type_panel(
      SAMPLING_TYPE_CLUSTER,
      "Cluster sampling",
      "cluster",
      "guide/type-cluster.md"
    )
  )
}

# A sampling type of the guide: its name, the name it had before, its text
guide_type_panel <- function(type, before, value, file) {
  accordion_panel(
    tagList(
      type,
      span(
        class = "badge text-bg-light fw-normal ms-2",
        paste("before:", before)
      )
    ),
    value = value,
    guide_text(file)
  )
}

# The links of the guide: to its own sections, to the types and to the app.
# `parent` is the session of the app, whose main tabs the guide can switch.
guide_server <- function(id, parent) {
  moduleServer(id, function(input, output, session) {
    section_links <- c(
      to_data = "data",
      to_parameters = "parameters",
      to_results = "results",
      to_news = "news"
    )
    type_links <- c(
      open_simple_random = "simple_random",
      open_pps = "pps",
      open_cluster = "cluster"
    )
    lapply(names(section_links), function(link) {
      observeEvent(
        input[[link]],
        bslib::nav_select("nav", section_links[[link]])
      )
    })
    lapply(names(type_links), function(link) {
      observeEvent(input[[link]], {
        bslib::nav_select("nav", "types")
        bslib::accordion_panel_set("types_accordion", type_links[[link]])
      })
    })
    observeEvent(
      input$to_app,
      bslib::nav_select("main_nav", "Sampling frame", session = parent)
    )
  })
}
