#' Query a Notion data source
#'
#' @param data_source_id Notion data source (database) ID.
#' @param filter Optional filter list.
#' @param sorts Optional sorts list.
#' @param page_size Page size (max 100).
#' @param start_cursor Pagination cursor.
#' @param token Notion API token.
#' @param notion_version Notion API version.
#' @export
#' @examples
#' \dontrun{
#' df <- notion_ds_query("DATA_SOURCE_ID")
#' }
notion_ds_query <- function(
  data_source_id,
  filter = NULL,
  sorts = NULL,
  page_size = 100,
  start_cursor = NULL,
  token = notion_token(),
  notion_version = .notion_default_version()
) {
  body <- list(page_size = page_size)
  if (!is.null(filter)) body$filter <- filter
  if (!is.null(sorts)) body$sorts <- sorts
  if (!is.null(start_cursor)) body$start_cursor <- start_cursor

  resp <- .notion_request(
    method = "POST",
    path = paste0("/data_sources/", data_source_id, "/query"),
    body = body,
    token = token,
    notion_version = notion_version
  )
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Query all pages in a data source
#'
#' @inheritParams notion_ds_query
#' @return A list of page objects.
#' @export
notion_ds_query_all <- function(...) {
  results <- list()
  cursor <- NULL

  repeat {
    resp <- notion_ds_query(..., start_cursor = cursor)
    results <- c(results, resp$results %||% list())
    if (isTRUE(resp$has_more)) {
      cursor <- resp$next_cursor
    } else {
      break
    }
  }

  results
}
