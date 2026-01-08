#' Convert Notion Property to R Value
#'
#' @param prop Notion property list.
#' @return R value.
#' @export
notion_prop_value <- function(prop) {
  type <- prop$type

  switch(
    type,
    title = .rich_text_concat(prop$title),
    rich_text = .rich_text_concat(prop$rich_text),
    number = prop$number %||% NA_real_,
    select = prop$select$name %||% NA_character_,
    multi_select = .multi_select_values(prop$multi_select),
    date = .date_value(prop$date),
    checkbox = prop$checkbox %||% NA,
    url = prop$url %||% NA_character_,
    email = prop$email %||% NA_character_,
    phone_number = prop$phone_number %||% NA_character_,
    created_time = prop$created_time %||% NA_character_,
    last_edited_time = prop$last_edited_time %||% NA_character_,
    formula = .formula_value(prop$formula),
    relation = {
      # TODO: relation expansion (return list-column of related ids)
      NA_character_
    },
    people = {
      # TODO: people expansion (return list-column of people objects)
      NA_character_
    },
    rollup = {
      # TODO: rollup expansion (return list-column of rollup values)
      NA_character_
    },
    NA
  )
}

.rich_text_concat <- function(items) {
  if (is.null(items) || length(items) == 0) {
    return(NA_character_)
  }
  parts <- vapply(items, function(item) item$plain_text %||% "", character(1))
  text <- paste(parts, collapse = "")
  if (identical(text, "")) NA_character_ else text
}

.multi_select_values <- function(items) {
  if (is.null(items) || length(items) == 0) {
    return(list(character()))
  }
  list(vapply(items, function(item) item$name %||% "", character(1)))
}

.date_value <- function(date) {
  if (is.null(date)) {
    return(NA_character_)
  }
  date$start %||% NA_character_
}

.formula_value <- function(formula) {
  if (is.null(formula)) {
    return(NA)
  }
  formula_type <- formula$type
  switch(
    formula_type,
    string = formula$string %||% NA_character_,
    number = formula$number %||% NA_real_,
    boolean = formula$boolean %||% NA,
    date = .date_value(formula$date),
    NA
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
