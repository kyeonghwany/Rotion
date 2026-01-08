#' Filter helpers for Notion data source queries
#'
#' @param property Property name.
#' @param value Filter value.
#' @return A filter list payload.
#' @export
#' @examples
#' filter_text_contains("제목", "hello")
filter_text_contains <- function(property, value) {
  list(property = property, rich_text = list(contains = value))
}

#' @rdname filter_text_contains
#' @export
filter_text_equals <- function(property, value) {
  list(property = property, rich_text = list(equals = value))
}

#' @rdname filter_text_contains
#' @export
filter_number_equals <- function(property, value) {
  list(property = property, number = list(equals = value))
}

#' @rdname filter_text_contains
#' @export
filter_select_equals <- function(property, value) {
  list(property = property, select = list(equals = value))
}

#' @rdname filter_text_contains
#' @export
filter_multi_select_contains <- function(property, value) {
  list(property = property, multi_select = list(contains = value))
}

#' @rdname filter_text_contains
#' @export
filter_checkbox_equals <- function(property, value) {
  list(property = property, checkbox = list(equals = value))
}

#' @rdname filter_text_contains
#' @export
filter_date_on_or_after <- function(property, value) {
  list(property = property, date = list(on_or_after = value))
}

#' @rdname filter_text_contains
#' @export
filter_date_on_or_before <- function(property, value) {
  list(property = property, date = list(on_or_before = value))
}

#' Combine filters with AND
#'
#' @param ... Filter payloads.
#' @return A combined filter list.
#' @export
filter_and <- function(...) {
  filters <- list(...)
  list(and = filters)
}

#' Combine filters with OR
#'
#' @param ... Filter payloads.
#' @return A combined filter list.
#' @export
filter_or <- function(...) {
  filters <- list(...)
  list(or = filters)
}

#' Sort ascending
#'
#' @param property Property name.
#' @return A sort payload.
#' @export
sort_asc <- function(property) {
  list(property = property, direction = "ascending")
}

#' Sort descending
#'
#' @param property Property name.
#' @return A sort payload.
#' @export
sort_desc <- function(property) {
  list(property = property, direction = "descending")
}
