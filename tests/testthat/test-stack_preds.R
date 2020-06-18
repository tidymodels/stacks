context("stack_preds")

preds_1 <- st_1 %>%
  stack_preds(mtcars)

preds_2 <- st_2 %>%
  stack_preds(mtcars)

test_that("output is a tibble", {
  expect_true(inherits(preds_1, "tbl_df"))
  expect_true(inherits(preds_2, "tbl_df"))
})

test_that("dimensionality", {
  expect_equal(nrow(preds_1), nrow(preds_2))
  expect_false(ncol(preds_1) == ncol(preds_2))
})
