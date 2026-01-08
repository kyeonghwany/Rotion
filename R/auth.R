#' Retrieve the Notion API token
#'
#' @return A character token string.
#' @export
#' @examples
#' \dontrun{
#' Sys.setenv(NOTION_TOKEN = "secret")
#' notion_token()
#' }
notion_token <- function() {
  token <- Sys.getenv("NOTION_TOKEN")
  if (identical(token, "")) {
    stop("NOTION_TOKEN is not set. Please set it via Sys.setenv().", call. = FALSE)
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
  req <- httr2::request(url)
  req <- httr2::req_method(req, method)
  req <- do.call(httr2::req_headers, c(list(req), .notion_headers(token, notion_version)))
  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  }
  .notion_perform(req)
}

.notion_perform <- function(req) {
  max_retries <- 5
  network_retries <- 3
  attempt <- 0
  net_attempt <- 0

  repeat {
    attempt <- attempt + 1
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(err) {
        net_attempt <<- net_attempt + 1
        if (net_attempt <= network_retries) {
          Sys.sleep(2 ^ (net_attempt - 1))
          return(NULL)
        }
        stop("Network error after retries: ", conditionMessage(err), call. = FALSE)
      }
    )

    if (is.null(resp)) {
      next
    }

    status <- httr2::resp_status(resp)
    if (status == 429 && attempt <= max_retries) {
      retry_after <- httr2::resp_header(resp, "Retry-After")
      wait <- if (!is.null(retry_after) && retry_after != "") {
        as.numeric(retry_after)
      } else {
        2 ^ (attempt - 1)
      }
      Sys.sleep(wait)
      next
    }

    if (status >= 400) {
      body_text <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
      preview <- substr(body_text, 1, 200)
      stop(
        sprintf("Notion API error (%s): %s", status, preview),
        call. = FALSE
      )
    }

    return(resp)
  }
}
