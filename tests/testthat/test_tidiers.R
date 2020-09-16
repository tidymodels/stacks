context("tidiers")

skip_if_not_installed("broom")
library(broom)
load(test_path("helper_data.Rda"))

test_that("tidy.model_stack behaves appropriately without needed tidiers", {
  expect_message(tidy_st_reg_1__ <- tidy(st_reg_1__))
  expect_message(tidy_st_class_1__ <- tidy(st_class_1__))
  
  expect_true(nrow(tidy_st_reg_1__) == 0)
  expect_true(nrow(tidy_st_class_1__) == 0)
})

test_that("tidy.model_stack works", {
  test_stack_reg <- 
    stacks() %>%
    add_candidates(reg_res_sp) %>%
    blend_predictions() %>%
    fit_members()
  
  tidy_st <- tidy(test_stack_reg)
  
  expect_true(nrow(tidy_st) == 4)
  expect_true(ncol(tidy_st) == 13)
  expect_true(all(
    colnames(tidy_st) == c("member", "r.squared", "adj.r.squared", "sigma", 
                           "statistic", "p.value", "df", "logLik", "AIC", 
                           "BIC", "deviance", "df.residual", "nobs")
  ))
})
