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

  rows <- .notion_normalize_rows(rows)
  dplyr::bind_rows(purrr::map(rows, tibble::as_tibble))
}

.notion_normalize_rows <- function(rows) {
  if (length(rows) == 0) {
    return(rows)
  }

  columns <- unique(unlist(purrr::map(rows, names)))
  numeric_types <- c("logical", "integer", "double")

  for (column in columns) {
    values <- purrr::map(rows, function(row) row[[column]])
    values <- purrr::keep(values, function(value) !is.null(value))
    if (length(values) == 0) {
      next
    }

    if (!all(purrr::map_lgl(values, function(value) is.atomic(value) && !is.list(value)))) {
      next
    }

    types <- unique(purrr::map_chr(values, typeof))
    if (length(types) <= 1 || all(types %in% numeric_types)) {
      next
    }

    rows <- purrr::map(rows, function(row) {
      value <- row[[column]]
      if (!is.null(value)) {
        row[[column]] <- as.character(value)
      }
      row
    })
  }

  rows
}
