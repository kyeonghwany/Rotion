#' Query a Notion data source
#'
#' @param data_source_id Notion data source (database) id.
#' @param filter Optional filter payload.
#' @param sorts Optional sorts payload.
#' @param page_size Page size (max 100).
#' @param start_cursor Optional cursor for pagination.
#' @param token Notion API token.
#' @param notion_version Notion API version.
#' @return Parsed JSON response as a list.
#' @export
#' @examples
#' \dontrun{
#' df <- notion_ds_query("DATA_SOURCE_ID")
#' }
notion_ds_query <- function(data_source_id,
                            filter = NULL,
                            sorts = NULL,
                            page_size = 100,
                            start_cursor = NULL,
                            token = notion_token(),
                            notion_version = .notion_version_default()) {
  if (is.null(data_source_id) || identical(data_source_id, "")) {
    rlang::abort("data_source_id는 필수입니다.")
  }

  body <- list(page_size = page_size)
  if (!is.null(filter)) {
    body$filter <- filter
  }
  if (!is.null(sorts)) {
    body$sorts <- sorts
  }
  if (!is.null(start_cursor)) {
    body$start_cursor <- start_cursor
  }

  resp <- .notion_request(
    path = sprintf("/data_sources/%s/query", data_source_id),
    method = "POST",
    body = body,
    token = token,
    notion_version = notion_version
  )

  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Query all pages from a Notion data source
#'
#' @inheritParams notion_ds_query
#' @return List of page objects (results only).
#' @export
notion_ds_query_all <- function(data_source_id,
                                filter = NULL,
                                sorts = NULL,
                                page_size = 100,
                                start_cursor = NULL,
                                token = notion_token(),
                                notion_version = .notion_version_default()) {
  results <- list()
  cursor <- start_cursor
  has_more <- TRUE

  while (isTRUE(has_more)) {
    resp <- notion_ds_query(
      data_source_id = data_source_id,
      filter = filter,
      sorts = sorts,
      page_size = page_size,
      start_cursor = cursor,
      token = token,
      notion_version = notion_version
    )

    results <- c(results, resp$results %||% list())
    has_more <- isTRUE(resp$has_more)
    cursor <- resp$next_cursor %||% NULL
  }

  results
}
