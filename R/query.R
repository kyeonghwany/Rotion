#' Query a Notion Data Source
#'
#' @param data_source_id Notion data source (database) ID.
#' @param filter Optional filter payload.
#' @param sorts Optional sorts payload.
#' @param page_size Page size for pagination.
#' @param start_cursor Cursor for pagination.
#' @param token Notion token.
#' @param notion_version Notion API version.
#'
#' @return Parsed JSON list from the Notion API.
#' @export
notion_ds_query <- function(
    data_source_id,
    filter = NULL,
    sorts = NULL,
    page_size = 100,
    start_cursor = NULL,
    token = notion_token(),
    notion_version = .notion_default_version()
) {
  body <- list(
    page_size = page_size,
    filter = filter,
    sorts = sorts,
    start_cursor = start_cursor
  )
  body <- body[!vapply(body, is.null, logical(1))]

  path <- paste0("/data_sources/", data_source_id, "/query")
  .notion_request("POST", path, body = body, token = token, notion_version = notion_version)
}

#' Query All Pages from a Notion Data Source
#'
#' @inheritParams notion_ds_query
#'
#' @return List of page results.
#' @export
notion_ds_query_all <- function(
    data_source_id,
    filter = NULL,
    sorts = NULL,
    page_size = 100,
    start_cursor = NULL,
    token = notion_token(),
    notion_version = .notion_default_version()
) {
  results <- list()
  cursor <- start_cursor

  repeat {
    response <- notion_ds_query(
      data_source_id = data_source_id,
      filter = filter,
      sorts = sorts,
      page_size = page_size,
      start_cursor = cursor,
      token = token,
      notion_version = notion_version
    )

    if (!is.null(response$results)) {
      results <- c(results, response$results)
    }

    if (isTRUE(response$has_more)) {
      cursor <- response$next_cursor
    } else {
      break
    }
  }

  results
}
