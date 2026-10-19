# The guide: its Markdown texts, and the cards and steps that show them.

#' Get the label of a column of the tables
#'
#' The code names its columns in snake_case; the tables show the labels.
#'
#' @param key The name the code gives the column.
#' @returns The label, as a string. A column without a label is an error.
#' @noRd
column_label <- function(key) {
  label <- names(COLUMN_LABELS)[match(key, COLUMN_LABELS)]
  if (is.na(label)) {
    stop("No label for the column: ", key, call. = FALSE)
  }
  label
}

#' Get what one row of the frame is, for a type of sampling
#'
#' @param type The key of the type: `"simple"`, `"pps"` or `"cluster"`.
#' @param detail Whether to give the longer form, for the page of the type.
#' @returns The text, as a string. An unknown type is an error.
#' @noRd
one_row <- function(type, detail = FALSE) {
  texts <- if (detail) ONE_ROW_DETAIL else ONE_ROW
  if (!type %in% names(texts)) {
    stop("No type of sampling named: ", type, call. = FALSE)
  }
  unname(texts[[type]])
}

#' Get the name of a sheet of the Excel file
#'
#' @param key The name the code gives the sheet, a name of `OUTPUT_SHEETS`.
#' @returns The name of the sheet, as a string. An unknown sheet is an error.
#' @noRd
sheet_name <- function(key) {
  if (!key %in% names(OUTPUT_SHEETS)) {
    stop("No sheet named: ", key, call. = FALSE)
  }
  unname(OUTPUT_SHEETS[[key]])
}

#' Get the label of a field of the form
#'
#' @param field The name of the field in the settings, see `FIELD_LABELS`.
#' @returns The label, as a string. A field without a label is an error.
#' @noRd
field_label <- function(field) {
  if (!field %in% names(FIELD_LABELS)) {
    stop("No label for the field: ", field, call. = FALSE)
  }
  unname(FIELD_LABELS[[field]])
}

#' Get what a placeholder of a guide text stands for
#'
#' A placeholder is the value of a constant (`{{MAX_UPLOAD_MB}}`), the label of a
#' column (`{{label:clusters_drawn}}`), the label of a field of the form
#' (`{{field:buffer}}`), the name of a sheet of the Excel file (`{{sheet:inputs}}`)
#' or the title of a check
#' (`{{check_title:exceeds_population}}`).
#'
#' @param name What is between the braces.
#' @returns The value, as a string. An unknown name is an error.
#' @noRd
guide_value <- function(name) {
  if (startsWith(name, "label:")) {
    return(column_label(sub("^label:", "", name)))
  }
  if (startsWith(name, "one_row:")) {
    return(one_row(sub("^one_row:", "", name), detail = FALSE))
  }
  if (startsWith(name, "one_row_detail:")) {
    return(one_row(sub("^one_row_detail:", "", name), detail = TRUE))
  }
  if (startsWith(name, "sheet:")) {
    return(sheet_name(sub("^sheet:", "", name)))
  }
  if (startsWith(name, "field:")) {
    return(field_label(sub("^field:", "", name)))
  }
  if (startsWith(name, "check_title:")) {
    return(check_title(sub("^check_title:", "", name)))
  }
  if (!exists(name)) {
    stop("Unknown {{", name, "}} in a guide text", call. = FALSE)
  }
  as.character(get(name))
}

#' Replace the placeholders of a guide text
#'
#' Every `{{...}}` is replaced by its value, so that a name or a limit that the
#' guide mentions is the one of the code, not a copy of it.
#'
#' @param text The Markdown text.
#' @returns The text with its placeholders filled in.
#' @noRd
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

#' Show a Markdown text of the guide
#'
#' The texts are files in `guide/`, and `NEWS.md`. Their placeholders are filled in
#' by `fill_guide_placeholders()`; `NEWS.md` has none, as the release notes tell
#' what the names were at the time. `markdown::mark()` turns `$...$` into `\(...\)`
#' for MathJax, unlike `shiny::markdown()`.
#'
#' @param text The Markdown text.
#' @returns A `div` with the HTML of the text.
#' @noRd
guide_markdown <- function(text) {
  text <- fill_guide_placeholders(text)
  # markdown::mark() turns $...$ into \(...\) for MathJax, unlike shiny::markdown()
  div(
    class = "guide-text",
    HTML(markdown::mark(text = text, format = "html", template = FALSE))
  )
}

#' Show a Markdown file of the guide
#'
#'
#'
#' @param file The path of the file.
#' @returns A `div` with the HTML of the file.
#' @noRd
guide_text <- function(file) {
  guide_markdown(paste(readLines(file, warn = FALSE), collapse = "\n"))
}

#' Show a Markdown file as one accordion panel per section
#'
#' One panel per second level heading; the first starts open. What comes before the
#' first heading is shown above the panels, as a discreet note.
#'
#' @param file The path of the file.
#' @param id The id of the accordion.
#' @returns The introduction and the accordion.
#' @noRd
guide_sections <- function(file, id) {
  lines <- readLines(file, warn = FALSE)
  starts <- grep("^## ", lines)
  intro <- if (starts[1] > 1) lines[seq_len(starts[1] - 1)] else character()
  ends <- c(starts[-1] - 1, length(lines))
  panels <- lapply(seq_along(starts), function(i) {
    body <- lines[(starts[i] + 1):ends[i]]
    guide_panel_text(
      sub("^## ", "", lines[starts[i]]),
      paste0("section", i),
      paste(body, collapse = "\n")
    )
  })
  tagList(
    if (length(intro) > 0 && any(nzchar(intro))) {
      # a discreet note above the panels
      div(
        class = "text-muted fst-italic small mb-2",
        guide_markdown(paste(intro, collapse = "\n"))
      )
    },
    do.call(guide_accordion, c(list(id), panels, list(open = "section1")))
  )
}

#' Show the checks of the app
#'
#' One closed panel per group of checks of `guide/checks.yml`, in an accordion that
#' goes inside a panel of the accordion of the page.
#'
#' @param ns The namespace function of the guide module.
#' @returns An accordion.
#' @noRd
guide_checks <- function(ns) {
  panels <- lapply(names(CHECKS$groups), function(id) {
    accordion_panel(
      CHECKS$groups[[id]]$title,
      value = id,
      guide_markdown(guide_checks_markdown(id))
    )
  })
  do.call(
    accordion,
    c(list(id = ns("checks_accordion"), open = FALSE, multiple = TRUE), panels)
  )
}

#' Build the Markdown of a group of checks
#'
#' When the group is checked, the table of its messages and what to do, then its
#' note. A check that only warns is not in the table: the note of the group tells
#' about it.
#'
#' @param group_id The identifier of the group, a key of `groups` in the file.
#' @returns The Markdown, as a string.
#' @noRd
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

#' Show a step of the How it works row
#'
#' The link, when there is one, sits at the bottom of the card.
#'
#' @param number The number of the step.
#' @param title The title of the step.
#' @param text The text of the step.
#' @param link A link to the part of the guide that tells more, or `NULL`.
#' @returns A card.
#' @noRd
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

#' Show a sampling type of the Three sampling types row
#'
#' The link sits at the bottom of the card.
#'
#' @param name The name of the type.
#' @param before The name it had before this version.
#' @param one_row What one row of the frame is.
#' @param needs What the type needs in the frame.
#' @param button_id The id of the link to the details.
#' @returns A card.
#' @noRd
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

#' Build the landing of the guide
#'
#' The title, the four steps, the three types and the note for the users of the
#' previous version.
#'
#' @param ns The namespace function of the guide module.
#' @returns The content of the Start pill.
#' @noRd
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
        "Draws the sample in the Sampling tab, then download one Excel file with the inputs, the summary and the sample.",
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
        paste0(ONE_ROW[["simple"]], "."),
        "no population column.",
        ns("open_simple_random")
      ),
      guide_type_card(
        SAMPLING_TYPE_PPS,
        "Simple random - allocation",
        paste0(ONE_ROW[["pps"]], "."),
        "a population column.",
        ns("open_pps")
      ),
      guide_type_card(
        SAMPLING_TYPE_CLUSTER,
        "Cluster sampling",
        paste0(ONE_ROW[["cluster"]], "."),
        "a cluster column and a population column.",
        ns("open_cluster")
      )
    ),
    div(
      class = "alert alert-info mt-3 mb-0 py-2 d-flex justify-content-between align-items-center",
      span(
        icon("circle-info"),
        strong(" Note. "),
        HTML(
          "This version changes the names of the sampling types and of the columns, the output (one Excel file instead of two CSV files) and the <strong>cluster sampling method</strong>."
        )
      ),
      actionLink(
        ns("to_news"),
        tagList("What's new ", icon("arrow-right")),
        class = "ms-3 text-nowrap"
      )
    )
  )
}

#' Build a panel of a guide accordion from a file
#'
#' @param title The title of the panel.
#' @param value The identifier of the panel, to say which panels start open.
#' @param file The Markdown file of its text.
#' @returns An accordion panel.
#' @noRd
guide_panel <- function(title, value, file) {
  accordion_panel(title, value = value, guide_text(file))
}

#' Build a panel of a guide accordion from a text
#'
#' @param title The title of the panel.
#' @param value The identifier of the panel, to say which panels start open.
#' @param text The Markdown text of the panel.
#' @returns An accordion panel.
#' @noRd
guide_panel_text <- function(title, value, text) {
  accordion_panel(title, value = value, guide_markdown(text))
}

#' Build an accordion of the guide
#'
#' Several panels can be open at once. The detail that is looked up, not read, starts
#' closed.
#'
#' @param id The id of the accordion.
#' @param open The identifiers of the panels that start open.
#' @param ... The panels, see `guide_panel()`.
#' @returns An accordion.
#' @noRd
guide_accordion <- function(id, ..., open) {
  accordion(id = id, open = open, multiple = TRUE, ...)
}

#' Build the Guide tab, the UI of the guide module
#'
#' One pill per section. Its links, to its own sections and to the app, are
#' handled by `guide_server()`.
#'
#' @param id The id of the module.
#' @returns A `nav_panel`.
#' @noRd
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
            guide_accordion(
              ns("data_accordion"),
              open = "file",
              guide_panel("The file", "file", "guide/data-file.md"),
              guide_panel(
                "What each type expects",
                "types",
                "guide/data-types.md"
              ),
              # an accordion of its own, one panel per group of checks
              accordion_panel(
                "What the tool checks",
                value = "checks",
                guide_checks(ns)
              )
            )
          ),
          nav_panel(
            "Parameters",
            value = "parameters",
            guide_accordion(
              ns("parameters_accordion"),
              open = "mode",
              guide_panel(
                "Mode of sampling",
                "mode",
                "guide/parameters-mode.md"
              ),
              guide_panel("Parameters", "list", "guide/parameters-list.md"),
              guide_panel(
                "Which parameters apply",
                "applies",
                "guide/parameters-applies.md"
              )
            )
          ),
          nav_panel(
            "Results",
            value = "results",
            guide_accordion(
              ns("results_accordion"),
              open = "tables",
              guide_panel("The tables", "tables", "guide/results-tables.md"),
              guide_panel("The Excel file", "file", "guide/results-file.md"),
              guide_panel(
                "Columns added by the tool",
                "columns",
                "guide/results-columns.md"
              ),
              guide_panel(
                "Sample summary",
                "summary",
                "guide/results-summary.md"
              )
            )
          ),
          nav_panel(
            "What's new",
            value = "news",
            guide_sections("NEWS.md", ns("news_accordion"))
          ),
          nav_panel(
            "Methods",
            value = "methods",
            guide_sections("guide/methods.md", ns("methods_accordion"))
          )
        )
      )
    )
  )
}

#' Show the three sampling types
#'
#' One panel each, closed until opened.
#'
#' @param ns The namespace function of the guide module.
#' @returns An accordion.
#' @noRd
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

#' Show a sampling type of the guide
#'
#' Its name, the name it had before, and its text.
#'
#' @param type The name of the type.
#' @param before The name it had before this version.
#' @param value The value of the panel, used to open it.
#' @param file The path of the file with its text.
#' @returns An accordion panel.
#' @noRd
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

#' Handle the links of the guide, the server of the guide module
#'
#' The links lead to the sections of the guide, to the types and to the app.
#'
#' @param id The id of the module.
#' @param parent The session of the app, whose main tabs the guide can switch.
#' @returns Nothing, it is called for its observers.
#' @noRd
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
