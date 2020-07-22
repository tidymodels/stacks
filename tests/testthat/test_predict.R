context("predict")

test_that("predict method works (regression)", {
  pred_r <- predict(st_reg_1__, penguins_test)
  pred_r2 <- predict(st_reg_1__, penguins_test, members = TRUE)
  
  expect_equal(names(pred_r), ".pred")
  expect_equal(nrow(pred_r), nrow(penguins_test))
  expect_true(check_inherits(pred_r, "tbl_df"))
  expect_true(check_inherits(pred_r$.pred, "numeric"))
  
  expect_equal(nrow(pred_r2), nrow(penguins_test))
  expect_true(check_inherits(pred_r2, "tbl_df"))
  expect_true(all(colnames(pred_r2 %in% names(st_reg_1__$member_fits))))
})

test_that("predict method works (classification)", {
  pred_c <- predict(st_class_1__, penguins_test)
  pred_l <- predict(st_log_1__, penguins_test)
  pred_c2 <- predict(st_class_1__, penguins_test, members = TRUE)
  pred_l2 <- predict(st_log_1__, penguins_test, members = TRUE)
  
  expect_equal(nrow(pred_c), nrow(penguins_test))
  expect_equal(nrow(pred_c), nrow(penguins_test))
  
  expect_equal(names(pred_c), ".pred_class")
  expect_equal(names(pred_l), ".pred_class")
  
  expect_true(all(grepl(".pred_class", names(pred_c2))))
  expect_true(all(grepl(".pred_class", names(pred_l2))))
  
  expect_true(all(grepl(
    paste0(names(st_class_1__$member_fits), collapse = "|"), 
    names(pred_c2)
  )))
  expect_true(all(grepl(
    paste0(names(st_log_1__$member_fits), collapse = "|"), 
    names(pred_l2)
  )))
})

test_that("predict method errors informatively", {
  skip("Soon. :-)")
  
  expect_true(TRUE)
})