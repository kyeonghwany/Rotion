.rich_text_plain <- function(items) {
  if (is.null(items) || length(items) == 0) {
    return(NA_character_)
  }
  text <- purrr::map_chr(items, ~ .x$plain_text %||% "")
  paste0(text, collapse = "")
}

.date_start <- function(date_obj) {
  if (is.null(date_obj)) {
    return(NA_character_)
  }
  date_obj$start %||% NA_character_
}

.formula_value <- function(formula) {
  if (is.null(formula) || is.null(formula$type)) {
    return(NA)
  }
  type <- formula$type
  if (type == "string") {
    return(formula$string %||% NA_character_)
  }
  if (type == "number") {
    return(formula$number %||% NA_real_)
  }
  if (type == "boolean") {
    return(formula$boolean %||% NA)
  }
  if (type == "date") {
    return(.date_start(formula$date))
  }
  NA
}

#' Coerce a Notion property value to an R value
#'
#' @param prop A Notion property object from the API response.
#' @return A scalar value or a list-column placeholder.
#' @export
notion_prop_value <- function(prop) {
  if (is.null(prop) || is.null(prop$type)) {
    return(NA)
  }

  type <- prop$type

  if (type == "title") {
    return(.rich_text_plain(prop$title))
  }
  if (type == "rich_text") {
    return(.rich_text_plain(prop$rich_text))
  }
  if (type == "number") {
    return(prop$number %||% NA_real_)
  }
  if (type == "select") {
    return(prop$select$name %||% NA_character_)
  }
  if (type == "multi_select") {
    values <- purrr::map_chr(prop$multi_select, ~ .x$name %||% NA_character_)
    return(list(values))
  }
  if (type == "date") {
    return(.date_start(prop$date))
  }
  if (type == "checkbox") {
    return(prop$checkbox %||% NA)
  }
  if (type %in% c("url", "email", "phone_number")) {
    return(prop[[type]] %||% NA_character_)
  }
  if (type %in% c("created_time", "last_edited_time")) {
    return(prop[[type]] %||% NA_character_)
  }
  if (type == "formula") {
    return(.formula_value(prop$formula))
  }
  if (type == "relation") {
    # TODO: expand relation support
    values <- purrr::map_chr(prop$relation, ~ .x$id %||% NA_character_)
    return(list(values))
  }
  if (type == "people") {
    # TODO: expand people support
    values <- purrr::map_chr(prop$people, ~ .x$id %||% NA_character_)
    return(list(values))
  }
  if (type == "rollup") {
    # TODO: expand rollup support
    return(list(prop$rollup %||% list()))
  }

  NA
}
