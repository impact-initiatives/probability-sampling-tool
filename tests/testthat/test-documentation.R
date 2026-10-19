# Every function of R/ is documented with a roxygen block (@noRd: the app is not
# a package, so no help file is made): a title, a description when useful, every
# parameter, what it returns. This checks the structure, not the wording.

r_files <- list.files(file.path(app_root, "R"), "\\.R$", full.names = TRUE)

# the top-level functions of a file: name, parameters, and the roxygen block
functions_of <- function(file) {
  lines <- readLines(file)
  exprs <- parse(file, keep.source = TRUE)
  refs <- attr(exprs, "srcref")
  found <- list()
  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    is_function <- is.call(expr) &&
      as.character(expr[[1]]) %in% c("<-", "=") &&
      is.name(expr[[2]]) &&
      is.call(expr[[3]]) &&
      identical(expr[[3]][[1]], as.name("function"))
    if (!is_function) {
      next
    }
    first <- refs[[i]][1]
    start <- first
    while (start > 1 && startsWith(lines[start - 1], "#")) {
      start <- start - 1
    }
    above <- lines[start:(first - 1)]
    found[[as.character(expr[[2]])]] <- list(
      parameters = as.character(names(expr[[3]][[2]])),
      block = sub("^#' ?", "", above[startsWith(above, "#'")])
    )
  }
  found
}

test_that("every function of R/ has a complete roxygen block", {
  checked <- 0
  for (file in r_files) {
    for (name in names(functions_of(file))) {
      fn <- functions_of(file)[[name]]
      info <- paste(basename(file), name)
      checked <- checked + 1

      expect_gt(length(fn$block), 0, label = paste(info, "has a roxygen block"))
      if (length(fn$block) == 0) {
        next
      }
      title <- fn$block[1]
      expect_true(
        nzchar(title) && !grepl("\\.$", title),
        info = paste(info, ": title")
      )
      expect_true(any(grepl("^@noRd", fn$block)), info = paste(info, ": @noRd"))
      expect_true(
        any(grepl("^@returns ", fn$block)),
        info = paste(info, ": @returns")
      )

      documented <- sub(
        "^@param (\\S+).*",
        "\\1",
        grep("^@param ", fn$block, value = TRUE)
      )
      expect_setequal(documented, fn$parameters)
    }
  }
  expect_gt(checked, 50)
})
