#' Filter helpers
#'
#' @param property Property name.
#' @param value Filter value.
#' @export
#' @examples
#' f <- filter_and(
#'   filter_select_equals("상태", "완납"),
#'   filter_date_on_or_after("예약일", "2026-01-01")
#' )
#' s <- list(sort_desc("예약일시"))
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
filter_date_on_or_after <- function(property, iso_date_or_datetime) {
  list(property = property, date = list(on_or_after = iso_date_or_datetime))
}

#' @rdname filter_text_contains
#' @export
filter_date_on_or_before <- function(property, iso_date_or_datetime) {
  list(property = property, date = list(on_or_before = iso_date_or_datetime))
}

#' Combine filters with AND
#'
#' @param ... Filter objects.
#' @export
filter_and <- function(...) {
  list(and = list(...))
}

#' Combine filters with OR
#'
#' @param ... Filter objects.
#' @export
filter_or <- function(...) {
  list(or = list(...))
}

#' Sort ascending by property
#'
#' @param property Property name.
#' @export
sort_asc <- function(property) {
  list(property = property, direction = "ascending")
}

#' Sort descending by property
#'
#' @param property Property name.
#' @export
sort_desc <- function(property) {
  list(property = property, direction = "descending")
}
