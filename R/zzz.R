#' Package options
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  opts <- list(
    notionds.base_url = "https://api.notion.com/v1",
    notionds.notion_version = "2025-09-03"
  )
  to_set <- opts[!names(opts) %in% names(options())]
  if (length(to_set)) {
    options(to_set)
  }
}

.notion_base_url <- function() {
  getOption("notionds.base_url", "https://api.notion.com/v1")
}

.notion_default_version <- function() {
  getOption("notionds.notion_version", "2025-09-03")
}
