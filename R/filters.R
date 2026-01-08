#' Filter helpers
#'
#' @export
filter_text_contains <- function(property, value) {
  list(property = property, rich_text = list(contains = value))
}

#' @export
filter_text_equals <- function(property, value) {
  list(property = property, rich_text = list(equals = value))
}

#' @export
filter_number_equals <- function(property, value) {
  list(property = property, number = list(equals = value))
}

#' @export
filter_select_equals <- function(property, value) {
  list(property = property, select = list(equals = value))
}

#' @export
filter_multi_select_contains <- function(property, value) {
  list(property = property, multi_select = list(contains = value))
}

#' @export
filter_checkbox_equals <- function(property, value) {
  list(property = property, checkbox = list(equals = value))
}

#' @export
filter_date_on_or_after <- function(property, iso_date_or_datetime) {
  list(property = property, date = list(on_or_after = iso_date_or_datetime))
}

#' @export
filter_date_on_or_before <- function(property, iso_date_or_datetime) {
  list(property = property, date = list(on_or_before = iso_date_or_datetime))
}

#' @export
filter_and <- function(...) {
  list(and = list(...))
}

#' @export
filter_or <- function(...) {
  list(or = list(...))
}

#' @export
sort_asc <- function(property) {
  list(property = property, direction = "ascending")
}

#' @export
sort_desc <- function(property) {
  list(property = property, direction = "descending")
}
