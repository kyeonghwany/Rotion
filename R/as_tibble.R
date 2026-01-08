#' Convert Notion data source results to a tibble
#'
#' @inheritParams notion_ds_query
#' @return A tibble with base page fields and property columns.
#' @export
#' @examples
#' \dontrun{
#' df <- notion_ds_tibble("DATA_SOURCE_ID")
#' }
notion_ds_tibble <- function(...) {
  pages <- notion_ds_query_all(...)
  if (length(pages) == 0) {
    return(tibble::tibble())
  }

  rows <- purrr::map(pages, function(page) {
    props <- page$properties %||% list()
    prop_values <- purrr::imap(props, function(prop, name) {
      notion_prop_value(prop)
    })
    c(
      list(
        id = page$id %||% NA_character_,
        created_time = page$created_time %||% NA_character_,
        last_edited_time = page$last_edited_time %||% NA_character_
      ),
      prop_values
    )
  })

  dplyr::bind_rows(purrr::map(rows, tibble::as_tibble))
}
