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

test_that("notion_prop_value returns NA for relation placeholder", {
  relation_prop <- list(type = "relation", relation = list())
  expect_true(is.na(notion_prop_value(relation_prop)))
})
