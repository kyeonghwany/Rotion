test_that("notion_ds_query_all paginates until done", {
  calls <- character()
  mock_query <- function(..., start_cursor = NULL) {
    calls <<- c(calls, if (is.null(start_cursor)) "NULL" else start_cursor)
    if (length(calls) == 1) {
      return(list(
        results = list(list(id = "page-1")),
        has_more = TRUE,
        next_cursor = "next-1"
      ))
    }
    list(
      results = list(list(id = "page-2")),
      has_more = FALSE,
      next_cursor = NULL
    )
  }

  testthat::local_mocked_bindings(notion_ds_query = mock_query)

  results <- notion_ds_query_all("ds")
  expect_length(results, 2)
  expect_identical(calls, c("NULL", "next-1"))
})
