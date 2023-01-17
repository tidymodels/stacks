if ((!on_cran()) || interactive()) {
  if (on_github()) {
    load(paste0(Sys.getenv("GITHUB_WORKSPACE"), "/tests/testthat/helper_data.Rda"))
  } else {
    load(test_path("helper_data.Rda"))
  }
}

skip_if_not_installed("modeldata")
library(modeldata)

skip_if_not_installed("ranger")
library(ranger)

skip_if_not_installed("kernlab")
library(kernlab)

skip_if_not_installed("nnet")
library(nnet)

test_that("predict method works (regression)", {
  skip_on_cran()
  
  pred_r <- predict(st_reg_1__, tree_frogs_reg_test)
  pred_r2 <- predict(st_reg_1__, tree_frogs_reg_test, members = TRUE)
  
  expect_equal(names(pred_r), ".pred")
  expect_equal(nrow(pred_r), nrow(tree_frogs_reg_test))
  expect_s3_class(pred_r, "tbl_df")
  expect_equal(class(pred_r$.pred), "numeric")
  
  expect_equal(nrow(pred_r2), nrow(tree_frogs_reg_test))
  expect_s3_class(pred_r2, "tbl_df")
  expect_true(all(colnames(pred_r2 %in% names(st_reg_1__$member_fits))))
})

test_that("predict method works (classification)", {
  skip_on_cran()
  
  pred_c <- predict(st_class_1__, tree_frogs_class_test)
  pred_l <- predict(st_log_1__, tree_frogs_class_test)
  pred_c2 <- predict(st_class_1__, tree_frogs_class_test, members = TRUE)
  pred_l2 <- predict(st_log_1__, tree_frogs_class_test, members = TRUE)
  
  expect_equal(nrow(pred_c), nrow(tree_frogs_class_test))
  expect_equal(nrow(pred_c), nrow(tree_frogs_class_test))
  
  expect_equal(names(pred_c), ".pred_class")
  expect_equal(names(pred_l), ".pred_class")
  
  expect_true(all(grepl(".pred_class", names(pred_c2))))
  expect_true(all(grepl(".pred_class", names(pred_l2))))
  
  expect_true(all(grepl(
    paste0(c(names(st_class_1__$member_fits), "pred_class"), collapse = "|"), 
    names(pred_c2)
  )))
  expect_true(all(grepl(
    paste0(c(names(st_log_1__$member_fits), "pred_class"), collapse = "|"), 
    names(pred_l2)
  )))
})

test_that("class probability summarization works", {
  skip_on_cran()
  
  pred_p <- predict(st_class_1__, tree_frogs_class_test, type = "prob")
  pred_c <- predict(st_class_1__, tree_frogs_class_test, type = "class")
  
  hard_class_preds <- 
    pred_p %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      dplyr::starts_with(".pred_"), 
      names_to = "level", 
      values_to = "prob"
    ) %>%
    dplyr::mutate(
      level = gsub(".pred_", "", level)
    ) %>%
    dplyr::group_by(
      row
    ) %>%
    dplyr::summarize(
      max = max(prob),
      level = level[prob == max]
    )

  expect_true(all(hard_class_preds$level == pred_c$.pred_class))  
})

test_that("predict method errors informatively", {
  skip_on_cran()
  
  expect_snapshot(error = TRUE,
    st_reg_1 %>% predict(penguins_test)
  )
  
  expect_snapshot(error = TRUE,
    st_reg_1_ %>% predict(penguins_test)
  )
  
  expect_snapshot(error = TRUE,
    st_reg_1__ %>% predict(penguins_test, members = "for sure!")
  )
  
  expect_snapshot(error = TRUE,
    st_reg_1__ %>% predict(penguins_test, opts = TRUE)
  )
})
