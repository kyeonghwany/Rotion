#' Package options
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  opts <- list(
    Rotion.base_url = "https://api.notion.com/v1",
    Rotion.notion_version = "2025-09-03"
  )
  to_set <- opts[!names(opts) %in% names(options())]
  if (length(to_set)) {
    options(to_set)
  }
}

.notion_base_url <- function() {
  getOption("Rotion.base_url", "https://api.notion.com/v1")
}

.notion_default_version <- function() {
  getOption("Rotion.notion_version", "2025-09-03")
}
