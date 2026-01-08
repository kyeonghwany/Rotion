.page_to_row <- function(page) {
  props <- page$properties %||% list()

  row <- list(
    id = page$id %||% NA_character_,
    created_time = page$created_time %||% NA_character_,
    last_edited_time = page$last_edited_time %||% NA_character_
  )

  values <- purrr::imap(props, ~ notion_prop_value(.x))
  row <- c(row, values)

  tibble::new_tibble(row, nrow = 1)
}

.pages_to_tibble <- function(pages) {
  if (length(pages) == 0) {
    return(tibble::tibble())
  }
  purrr::map_dfr(pages, .page_to_row)
}

#' Query a Notion data source and return a tibble
#'
#' @inheritParams notion_ds_query
#' @return A tibble with page fields and property columns.
#' @export
#' @examples
#' \dontrun{
#' df <- notion_ds_tibble("DATA_SOURCE_ID")
#'
#' f <- filter_and(
#'   filter_select_equals("상태", "완납"),
#'   filter_date_on_or_after("예약일", "2026-01-01")
#' )
#' df <- notion_ds_tibble("DATA_SOURCE_ID", filter = f)
#'
#' s <- list(sort_desc("예약일시"))
#' df <- notion_ds_tibble("DATA_SOURCE_ID", sorts = s)
#' }
notion_ds_tibble <- function(data_source_id,
                             filter = NULL,
                             sorts = NULL,
                             page_size = 100,
                             start_cursor = NULL,
                             token = notion_token(),
                             notion_version = .notion_version_default()) {
  pages <- notion_ds_query_all(
    data_source_id = data_source_id,
    filter = filter,
    sorts = sorts,
    page_size = page_size,
    start_cursor = start_cursor,
    token = token,
    notion_version = notion_version
  )

  .pages_to_tibble(pages)
}
