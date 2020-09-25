context("predict")

load(test_path("helper_data.Rda"))

test_that("predict method works (regression)", {
  pred_r <- predict(st_reg_1__, tree_frogs_reg_test)
  pred_r2 <- predict(st_reg_1__, tree_frogs_reg_test, members = TRUE)
  
  expect_equal(names(pred_r), ".pred")
  expect_equal(nrow(pred_r), nrow(tree_frogs_reg_test))
  expect_true(check_inherits(pred_r, "tbl_df"))
  expect_true(check_inherits(pred_r$.pred, "numeric"))
  
  expect_equal(nrow(pred_r2), nrow(tree_frogs_reg_test))
  expect_true(check_inherits(pred_r2, "tbl_df"))
  expect_true(all(colnames(pred_r2 %in% names(st_reg_1__$member_fits))))
})

test_that("predict method works (classification)", {
  pred_c <- predict(st_class_1__, tree_frogs_class_test)
  pred_l <- predict(st_log_1__, tree_frogs_class_test)
  pred_c2 <- predict(st_class_1__, tree_frogs_class_test, members = TRUE)
  # NOTE: members prediction erroring here
  # pred_l2 <- predict(st_log_1__, tree_frogs_class_test, members = TRUE)
  
  expect_equal(nrow(pred_c), nrow(tree_frogs_class_test))
  expect_equal(nrow(pred_c), nrow(tree_frogs_class_test))
  
  expect_equal(names(pred_c), ".pred_class")
  expect_equal(names(pred_l), ".pred_class")
  
  expect_true(all(grepl(".pred_class", names(pred_c2))))
  # expect_true(all(grepl(".pred_class", names(pred_l2))))
  
  expect_true(all(grepl(
    paste0(c(names(st_class_1__$member_fits), "pred_class"), collapse = "|"), 
    names(pred_c2)
  )))
  # expect_true(all(grepl(
  #   paste0(c(names(st_log_1__$member_fits), "pred_class"), collapse = "|"), 
  #   names(pred_l2)
  # )))
})

test_that("predict method errors informatively", {
  expect_error(
    st_reg_1 %>% predict(penguins_test),
    "supplied data stack must be evaluated with"
  )
  
  expect_error(
    st_reg_1_ %>% predict(penguins_test),
    "hasn't been fitted yet."
  )
  
  expect_error(
    st_reg_1__ %>% predict(penguins_test, members = "for sure!"),
    "needs to inherit from `logical`, but its class is `character`."
  )
  
  expect_error(
    st_reg_1__ %>% predict(penguins_test, opts = TRUE),
    "needs to inherit from `list`, but its class is `logical`."
  )
})
