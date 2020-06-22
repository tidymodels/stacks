context("stack_data")

preds_1 <- st_1 %>%
  stack_data(mtcars)

preds_2 <- st_2 %>%
  stack_data(mtcars)

test_that("output classes", {
  expect_true(inherits(preds_1, "stacked_data"))
  expect_true(inherits(preds_2, "stacked_data"))
  
  expect_true(inherits(preds_1$stacked_data, "tbl_df"))
  expect_true(inherits(preds_2$stacked_data, "tbl_df"))
})

test_that("dimensionality", {
  expect_equal(nrow(preds_1$stacked_data), nrow(preds_2$stacked_data))
  expect_false(ncol(preds_1$stacked_data) == ncol(preds_2$stacked_data))
})
