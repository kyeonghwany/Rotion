test_that("notion_prop_value coerces title and rich_text", {
  title_prop <- list(
    type = "title",
    title = list(
      list(plain_text = "Hello "),
      list(plain_text = "World")
    )
  )
  expect_equal(notion_prop_value(title_prop), "Hello World")

  rich_prop <- list(type = "rich_text", rich_text = list())
  expect_identical(notion_prop_value(rich_prop), NA_character_)
})

test_that("notion_prop_value coerces select and number", {
  select_prop <- list(type = "select", select = list(name = "Done"))
  number_prop <- list(type = "number", number = 12.5)

  expect_equal(notion_prop_value(select_prop), "Done")
  expect_equal(notion_prop_value(number_prop), 12.5)
})

test_that("notion_prop_value handles multi_select and formula", {
  multi_prop <- list(
    type = "multi_select",
    multi_select = list(
      list(name = "A"),
      list(name = "B")
    )
  )
  multi_val <- notion_prop_value(multi_prop)
  expect_true(is.list(multi_val))
  expect_identical(multi_val[[1]], c("A", "B"))

  formula_prop <- list(
    type = "formula",
    formula = list(type = "date", date = list(start = "2025-01-01"))
  )
  expect_identical(notion_prop_value(formula_prop), "2025-01-01")
})

test_that("notion_prop_value handles relation values", {
  relation_prop <- list(
    type = "relation",
    relation = list(
      list(id = "page-1"),
      list(id = "page-2")
    )
  )
  relation_val <- notion_prop_value(relation_prop)
  expect_true(is.list(relation_val))
  expect_identical(relation_val[[1]], c("page-1", "page-2"))
})

test_that("notion_prop_value handles rollup values", {
  rollup_number <- list(type = "rollup", rollup = list(type = "number", number = 4))
  expect_identical(notion_prop_value(rollup_number), 4)

  rollup_array <- list(
    type = "rollup",
    rollup = list(
      type = "array",
      array = list(
        list(type = "number", number = 1),
        list(type = "number", number = 2)
      )
    )
  )
  rollup_val <- notion_prop_value(rollup_array)
  expect_true(is.list(rollup_val))
  expect_identical(rollup_val[[1]], c(1, 2))

  rollup_empty_array <- list(
    type = "rollup",
    rollup = list(
      type = "array",
      array = list()
    )
  )
  rollup_empty_val <- notion_prop_value(rollup_empty_array)
  expect_true(is.list(rollup_empty_val))
  expect_identical(rollup_empty_val[[1]], list())
})
