# What the app knows about itself, to say in the output which version drew a sample.

#' Read the version of the app
#'
#' The version is `APP_VERSION`. The short commit is read from `BUILD_INFO`
#' (`Commit: abc1234`), a file the deployment writes: `.git` is not sent to the
#' server, so a local copy or a hand-made deployment has none. The app never
#' fails for lack of this file.
#'
#' @param root The folder of the app, where `BUILD_INFO` is.
#' @returns A list with `version`, `commit` (`NA` when unknown) and `label`, the
#'   two together, e.g. `"2026.1.0 (abc1234)"`.
#' @noRd
app_info <- function(root = ".") {
  commit <- tryCatch(
    unname(read.dcf(file.path(root, "BUILD_INFO"), fields = "Commit")[1, 1]),
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )
  commit <- if (is.na(commit) || !nzchar(trimws(commit))) {
    NA_character_
  } else {
    trimws(commit)
  }
  list(
    version = APP_VERSION,
    commit = commit,
    label = if (is.na(commit)) {
      APP_VERSION
    } else {
      paste0(APP_VERSION, " (", commit, ")")
    }
  )
}
