# The definition of the checks (guide/checks.yml): its structure, and that what
# the guide shows of a message is part of the message the code gives.

checks <- CHECKS$checks

test_that("every check has a known group, a message and what to do", {
  expect_gt(length(checks), 0)
  for (id in names(checks)) {
    check <- checks[[id]]
    expect_true(check$group %in% names(CHECKS$groups), info = id)
    expect_true(
      is.character(check$message) && nzchar(check$message),
      info = id
    )
    expect_true(is.character(check$fix) && nzchar(check$fix), info = id)
    expect_true(is.null(check$kind) || check$kind %in% "warning", info = id)
  }
  for (group in CHECKS$groups) {
    expect_true(nzchar(group$title) && nzchar(group$when))
  }
})

test_that("the placeholders of a message are well formed and fill in", {
  for (id in names(checks)) {
    message <- checks[[id]]$message
    # the labels of the fields are filled in first, and must be known
    fields <- regmatches(
      message,
      gregexpr("\\{\\{field:[^{}]*\\}\\}", message)
    )[[1]]
    expect_true(
      all(
        sub("^\\{\\{field:(.*)\\}\\}$", "\\1", fields) %in% names(FIELD_LABELS)
      ),
      info = id
    )
    message <- fill_field_labels(message)
    names_in <- regmatches(message, gregexpr("\\{[^{}]*\\}", message))[[1]]
    expect_true(all(grepl("^\\{[a-z_]+\\}$", names_in)), info = id)
    values <- stats::setNames(
      as.list(rep("X", length(names_in))),
      gsub("[{}]", "", names_in)
    )
    expect_false(
      grepl("\\{[a-z_]+\\}", fill_template(message, values)),
      info = id
    )
  }
  expect_equal(fill_template("a {x} b {y}", list(x = 1)), "a 1 b {y}")
})

test_that("what the guide shows of a message is part of the message", {
  for (id in names(checks)) {
    shown <- checks[[id]]$guide_message
    if (is.null(shown)) {
      next
    }
    # "…" and {{CONSTANT}} stand for parts that change
    fixed <- strsplit(shown, "…|\\{\\{[^}]+\\}\\}")[[1]]
    for (piece in fixed[nzchar(fixed)]) {
      expect_true(
        grepl(piece, checks[[id]]$message, fixed = TRUE),
        info = paste(id, ":", piece)
      )
    }
  }
})

test_that("the {{...}} of the guide texts of the checks can be filled in", {
  texts <- unlist(lapply(checks, function(check) {
    c(check$fix, check$guide_message)
  }))
  texts <- c(texts, unlist(lapply(CHECKS$groups, function(group) group$note)))
  for (text in texts) {
    expect_no_error(fill_guide_placeholders(text))
  }
})

test_that("an unknown check is an error, not an empty message", {
  expect_error(check_message("no_such_check"), "Unknown check")
  expect_error(check_text("no_such_text"), "Unknown text")
})
