#' notionds Package Options
#'
#' @keywords internal
notionds_options <- function() {
  list(
    base_url = "https://api.notion.com/v1",
    notion_version = "2025-09-03",
    max_retries = 5
  )
}

.notion_base_url <- function() {
  getOption("notionds.base_url", notionds_options()$base_url)
}

.notion_default_version <- function() {
  getOption("notionds.notion_version", notionds_options()$notion_version)
}

.notion_max_retries <- function() {
  getOption("notionds.max_retries", notionds_options()$max_retries)
}
