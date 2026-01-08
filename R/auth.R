#' Read Notion API Token
#'
#' Reads the Notion API token from the `NOTION_TOKEN` environment variable.
#'
#' @return A character scalar token.
#' @export
notion_token <- function() {
  token <- Sys.getenv("NOTION_TOKEN")
  if (identical(token, "")) {
    stop("NOTION_TOKEN is not set. Please set Sys.getenv('NOTION_TOKEN') or pass token.")
  }
  token
}

.notion_headers <- function(token, notion_version) {
  list(
    Authorization = paste("Bearer", token),
    "Notion-Version" = notion_version,
    "Content-Type" = "application/json"
  )
}

.notion_request <- function(method, path, body = NULL, token, notion_version) {
  url <- paste0(.notion_base_url(), path)
  req <- httr2::request(url) |>
    httr2::req_method(method) |>
    httr2::req_headers(!!!.notion_headers(token, notion_version))

  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body = body, auto_unbox = TRUE)
  }

  .notion_perform(req)
}

.notion_perform <- function(req) {
  max_tries <- .notion_max_retries()
  attempt <- 1
  repeat {
    result <- tryCatch(
      httr2::req_perform(req),
      error = function(err) err
    )

    if (inherits(result, "error")) {
      if (attempt < 3) {
        Sys.sleep(2 ^ (attempt - 1))
        attempt <- attempt + 1
        next
      }
      stop("Network error after retries: ", conditionMessage(result))
    }

    status <- httr2::resp_status(result)

    if (status == 429 && attempt < max_tries) {
      retry_after <- httr2::resp_headers(result)[["retry-after"]]
      wait <- if (!is.null(retry_after) && nzchar(retry_after)) {
        as.numeric(retry_after)
      } else {
        2 ^ (attempt - 1)
      }
      Sys.sleep(wait)
      attempt <- attempt + 1
      next
    }

    if (status >= 400) {
      body_text <- tryCatch(httr2::resp_body_string(result), error = function(err) "")
      snippet <- substr(body_text, 1, 500)
      stop("Notion API error (", status, "): ", snippet)
    }

    return(httr2::resp_body_json(result, simplifyVector = FALSE))
  }
}
