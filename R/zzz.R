#' Package options and defaults
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("notionds.base_url"))) {
    options(notionds.base_url = "https://api.notion.com/v1")
  }
  if (is.null(getOption("notionds.notion_version"))) {
    options(notionds.notion_version = "2025-09-03")
  }
}

.notion_base_url <- function() {
  getOption("notionds.base_url", "https://api.notion.com/v1")
}

.notion_version_default <- function() {
  getOption("notionds.notion_version", "2025-09-03")
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
