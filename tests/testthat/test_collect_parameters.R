context("collect_parameters")

load(test_path("helper_data.Rda"))

test_that("collect_parameters dispatch works", {
  expect_error(
    1 %>% collect_parameters(),
    "currently implemented for numeric objects"
  )
  
  expect_error(
    mtcars %>% collect_parameters(),
    "currently implemented for data.frame objects"
  )
})

test_that("collect_parameters errors informatively with bad arguments", {
  expect_error(
    st_reg_1 %>% collect_parameters("the first one"),
    "must be the name given"
  )
  
  expect_error(
    stacks() %>% collect_parameters("all of them"),
    "must be the name given"
  )
})

test_that("collect_parameters on a data stack works", {
  res <- collect_parameters(st_reg_1, "reg_res_svm")
  res2 <- collect_parameters(st_reg_2, "reg_res_sp")
  res3 <- 
    collect_parameters(
      stacks() %>% add_candidates(reg_res_lr, name = "lr"),
      "lr"
    )
  
  expect_true(check_inherits(res, "tbl_df"))
  expect_true(check_inherits(res2, "tbl_df"))
  expect_true(check_inherits(res3, "tbl_df"))
  
  expect_equal(ncol(res), 3)
  expect_equal(nrow(res), 5)
  
  expect_equal(ncol(res2), 2)
  expect_equal(nrow(res2), 9)
  
  expect_equal(ncol(res3), 1)
  expect_equal(nrow(res3), 1)
})

test_that("collect_parameters on a model stack works", {
  res <- collect_parameters(st_reg_1_, "reg_res_svm")
  res2 <- collect_parameters(st_reg_2 %>% blend_predictions(), "reg_res_sp")
  
  expect_true(check_inherits(res, "tbl_df"))
  
  expect_equal(ncol(res), 4)
  expect_equal(nrow(res), 5)
  
  expect_equal(ncol(res2), 3)
  expect_equal(nrow(res2), 9)
  
  expect_true(
    all(
      c("member", 
        dials::parameters(st_reg_1_$model_defs$reg_res_svm) %>% pull(id), 
        "coef") %in% 
      colnames(res)
    )
  )
})
