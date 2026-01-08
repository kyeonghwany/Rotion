library(testthat)

test_that("pagination aggregates results", {
  page1 <- list(
    results = list(list(id = "p1"), list(id = "p2")),
    has_more = TRUE,
    next_cursor = "cursor-2"
  )
  page2 <- list(
    results = list(list(id = "p3")),
    has_more = FALSE,
    next_cursor = NULL
  )

  calls <- 0
  local_mocked_bindings(
    notion_ds_query = function(...) {
      calls <<- calls + 1
      if (calls == 1) {
        page1
      } else {
        page2
      }
    }
  )

  results <- notion_ds_query_all("ds_123")
  expect_equal(length(results), 3)
  expect_equal(vapply(results, function(x) x$id, character(1)), c("p1", "p2", "p3"))
})
