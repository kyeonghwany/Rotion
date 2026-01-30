#' Coerce a Notion property value to an R value
#'
#' @param property A Notion property list.
#' @return An R scalar or list-column friendly value.
#' @export
notion_prop_value <- function(property) {
  type <- property$type %||% ""
  value <- property[[type]]
  switch(
    type,
    title = .notion_rich_text_plain(value),
    rich_text = .notion_rich_text_plain(value),
    number = value %||% NA_real_,
    select = .notion_select_value(value),
    multi_select = .notion_multi_select_value(value),
    date = .notion_date_value(value),
    checkbox = value %||% NA,
    url = value %||% NA_character_,
    email = value %||% NA_character_,
    phone_number = value %||% NA_character_,
    created_time = value %||% NA_character_,
    last_edited_time = value %||% NA_character_,
    formula = .notion_formula_value(value),
    relation = .notion_relation_value(value),
    people = {
      # TODO: expand people support
      NA
    },
    rollup = .notion_rollup_value(value),
    NA
  )
}

.notion_rich_text_plain <- function(items) {
  if (is.null(items) || length(items) == 0) {
    return(NA_character_)
  }
  texts <- purrr::map_chr(items, function(item) {
    item$plain_text %||% ""
  })
  paste0(texts, collapse = "")
}

.notion_select_value <- function(value) {
  if (is.null(value)) {
    return(NA_character_)
  }
  value$name %||% NA_character_
}

.notion_multi_select_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(list(character()))
  }
  list(purrr::map_chr(value, function(item) item$name %||% ""))
}

.notion_date_value <- function(value) {
  if (is.null(value)) {
    return(NA_character_)
  }
  value$start %||% NA_character_
}

.notion_formula_value <- function(value) {
  if (is.null(value)) {
    return(NA)
  }
  ftype <- value$type %||% ""
  switch(
    ftype,
    string = value$string %||% NA_character_,
    number = value$number %||% NA_real_,
    boolean = value$boolean %||% NA,
    date = .notion_date_value(value$date),
    NA
  )
}

.notion_relation_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(list(character()))
  }
  list(purrr::map_chr(value, function(item) item$id %||% ""))
}

.notion_rollup_value <- function(value) {
  if (is.null(value)) {
    return(NA)
  }
  rtype <- value$type %||% ""
  switch(
    rtype,
    number = value$number %||% NA_real_,
    date = .notion_date_value(value$date),
    array = {
      items <- value$array %||% list()
      if (length(items) == 0) {
        return(list(list()))
      }
      values <- purrr::map(items, notion_prop_value)
      if (all(purrr::map_lgl(values, function(item) is.atomic(item) && !is.list(item)))) {
        return(list(unlist(values, use.names = FALSE)))
      }
      list(values)
    },
    NA
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
