library(testthat)


test_that("title converts to string", {
  prop <- list(type = "title", title = list(list(plain_text = "Hello")))
  expect_equal(notion_prop_value(prop), "Hello")
})

test_that("rich_text combines", {
  prop <- list(type = "rich_text", rich_text = list(list(plain_text = "A"), list(plain_text = "B")))
  expect_equal(notion_prop_value(prop), "AB")
})

test_that("multi_select returns list column", {
  prop <- list(type = "multi_select", multi_select = list(list(name = "A"), list(name = "B")))
  expect_equal(notion_prop_value(prop), list(c("A", "B")))
})

test_that("formula returns number", {
  prop <- list(type = "formula", formula = list(type = "number", number = 3.5))
  expect_equal(notion_prop_value(prop), 3.5)
})

test_that("date returns start", {
  prop <- list(type = "date", date = list(start = "2026-01-01"))
  expect_equal(notion_prop_value(prop), "2026-01-01")
})
