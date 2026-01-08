library(testthat)

 test_that("notion_ds_query_all paginates results", {
  calls <- 0
  resp1 <- list(
    results = list(list(id = "1")),
    has_more = TRUE,
    next_cursor = "next"
  )
  resp2 <- list(
    results = list(list(id = "2"), list(id = "3")),
    has_more = FALSE,
    next_cursor = NULL
  )

  mock_query <- function(...) {
    calls <<- calls + 1
    if (calls == 1) {
      resp1
    } else {
      resp2
    }
  }

  results <- with_mocked_bindings(
    notion_ds_query = mock_query,
    notion_ds_query_all("db")
  )

  expect_equal(length(results), 3)
  expect_equal(vapply(results, `[[`, character(1), "id"), c("1", "2", "3"))
})
