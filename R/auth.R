#' Retrieve Notion API token
#'
#' Reads the `NOTION_TOKEN` environment variable.
#'
#' @return A character token string.
#' @export
#' @examples
#' \dontrun{
#' Sys.setenv(NOTION_TOKEN = "secret")
#' notion_token()
#' }
notion_token <- function() {
  token <- Sys.getenv("NOTION_TOKEN", unset = "")
  if (identical(token, "")) {
    rlang::abort("NOTION_TOKEN 환경변수를 설정해 주세요.")
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

.retry_after_seconds <- function(resp, attempt) {
  headers <- httr2::resp_headers(resp)
  retry_after <- suppressWarnings(as.numeric(headers[["retry-after"]]))
  if (!is.na(retry_after) && retry_after > 0) {
    return(retry_after)
  }
  2^(attempt - 1)
}

.request_error <- function(status, body) {
  snippet <- substr(body %||% "", 1, 500)
  rlang::abort(sprintf("Notion API request failed with status %s: %s", status, snippet))
}

.perform_with_retry <- function(req) {
  max_429 <- 5
  max_network <- 3
  attempt_429 <- 0
  attempt_network <- 0

  repeat {
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(err) {
        attempt_network <<- attempt_network + 1
        if (attempt_network >= max_network) {
          rlang::abort(
            sprintf("Network error after %s attempts: %s", attempt_network, err$message)
          )
        }
        Sys.sleep(2^(attempt_network - 1))
        structure(list(error = err), class = "notionds_retry_error")
      }
    )

    if (inherits(resp, "notionds_retry_error")) {
      next
    }

    status <- httr2::resp_status(resp)
    if (status == 429) {
      attempt_429 <- attempt_429 + 1
      if (attempt_429 >= max_429) {
        body <- httr2::resp_body_string(resp)
        .request_error(status, body)
      }
      Sys.sleep(.retry_after_seconds(resp, attempt_429))
      next
    }

    if (status >= 400) {
      body <- httr2::resp_body_string(resp)
      .request_error(status, body)
    }

    return(resp)
  }
}

.notion_request <- function(path,
                            method = "POST",
                            body = NULL,
                            token,
                            notion_version = .notion_version_default()) {
  url <- paste0(.notion_base_url(), path)
  req <- httr2::request(url) |>
    httr2::req_method(method) |>
    httr2::req_headers(!!!.notion_headers(token, notion_version))

  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body = body, auto_unbox = TRUE)
  }

  .perform_with_retry(req)
}
