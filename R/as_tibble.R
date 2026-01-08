#' Query Notion Data Source and Return Tibble
#'
#' @inheritParams notion_ds_query_all
#' @return Tibble of page properties.
#' @examples
#' Sys.setenv(NOTION_TOKEN = \"secret_abc123\")
#' \dontrun{
#' df <- notion_ds_tibble(\"DATA_SOURCE_ID\")
#'
#' f <- filter_and(
#'   filter_select_equals(\"상태\", \"완납\"),
#'   filter_date_on_or_after(\"예약일\", \"2026-01-01\")
#' )
#' df <- notion_ds_tibble(\"DATA_SOURCE_ID\", filter = f)
#'
#' s <- list(sort_desc(\"예약일시\"))
#' df <- notion_ds_tibble(\"DATA_SOURCE_ID\", sorts = s)
#' }
#' @export
notion_ds_tibble <- function(
    data_source_id,
    filter = NULL,
    sorts = NULL,
    page_size = 100,
    start_cursor = NULL,
    token = notion_token(),
    notion_version = .notion_default_version()
) {
  pages <- notion_ds_query_all(
    data_source_id = data_source_id,
    filter = filter,
    sorts = sorts,
    page_size = page_size,
    start_cursor = start_cursor,
    token = token,
    notion_version = notion_version
  )

  rows <- purrr::map(pages, function(page) {
    props <- page$properties
    values <- purrr::imap(props, function(prop, name) {
      notion_prop_value(prop)
    })

    tibble::tibble(
      id = page$id,
      created_time = page$created_time,
      last_edited_time = page$last_edited_time,
      !!!values
    )
  })

  dplyr::bind_rows(rows)
}
