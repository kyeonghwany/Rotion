library(testthat)

 test_that("title and rich_text coerce to strings", {
  title_prop <- list(type = "title", title = list(list(plain_text = "Hello")))
  rich_prop <- list(type = "rich_text", rich_text = list(list(plain_text = "World")))

  expect_equal(notion_prop_value(title_prop), "Hello")
  expect_equal(notion_prop_value(rich_prop), "World")
})

 test_that("multi_select returns list column", {
  prop <- list(
    type = "multi_select",
    multi_select = list(list(name = "A"), list(name = "B"))
  )
  expect_equal(notion_prop_value(prop), list(c("A", "B")))
})

 test_that("date and checkbox coerce", {
  prop_date <- list(type = "date", date = list(start = "2026-01-01"))
  prop_checkbox <- list(type = "checkbox", checkbox = TRUE)

  expect_equal(notion_prop_value(prop_date), "2026-01-01")
  expect_true(notion_prop_value(prop_checkbox))
})

 test_that("formula handles number and date", {
  prop_number <- list(type = "formula", formula = list(type = "number", number = 10))
  prop_date <- list(
    type = "formula",
    formula = list(type = "date", date = list(start = "2026-01-01"))
  )

  expect_equal(notion_prop_value(prop_number), 10)
  expect_equal(notion_prop_value(prop_date), "2026-01-01")
})
