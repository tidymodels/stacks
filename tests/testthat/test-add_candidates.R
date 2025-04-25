if ((!on_cran()) || interactive()) {
  if (on_github()) {
    load(paste0(
      Sys.getenv("GITHUB_WORKSPACE"),
      "/tests/testthat/helper_data.Rda"
    ))
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

skip_if_not_installed("yardstick")

test_that("stack can add candidates (regression)", {
  skip_on_cran()

  expect_equal(
    st_0 |> add_candidates(reg_res_svm),
    st_reg_1
  )

  expect_equal(
    st_reg_1 |> add_candidates(reg_res_sp),
    st_reg_2
  )

  expect_true(data_stack_constr(st_0))
  expect_true(data_stack_constr(st_reg_1))
  expect_true(data_stack_constr(st_reg_2))
})

test_that("stack can add candidates (multinomial classification)", {
  skip_on_cran()

  expect_equal(
    st_0 |> add_candidates(class_res_rf),
    st_class_1
  )

  expect_equal(
    st_class_1 |> add_candidates(class_res_nn),
    st_class_2
  )

  expect_true(data_stack_constr(st_class_1))
  expect_true(data_stack_constr(st_class_2))
})

test_that("stack can add candidates (two-way classification)", {
  skip_on_cran()

  expect_equal(
    st_0 |> add_candidates(log_res_rf),
    st_log_1
  )

  expect_equal(
    st_log_1 |> add_candidates(log_res_nn),
    st_log_2
  )

  expect_true(data_stack_constr(st_log_1))
  expect_true(data_stack_constr(st_log_2))
})

test_that("add_candidates errors informatively with bad arguments", {
  skip_on_cran()

  expect_snapshot(
    add_candidates(reg_res_svm, "svm"),
    error = TRUE
  )

  expect_snapshot(
    st_reg_2 |> add_candidates("howdy"),
    error = TRUE
  )

  expect_snapshot(
    stacks() |> add_candidates(reg_res_sp, reg_res_svm),
    error = TRUE
  )

  expect_snapshot(
    stacks() |> add_candidates(reg_res_sp, TRUE),
    error = TRUE
  )

  expect_snapshot(
    add_candidates("howdy", reg_res_svm),
    error = TRUE
  )

  expect_snapshot(
    st_reg_1 |> add_candidates(reg_res_svm),
    error = TRUE
  )

  expect_snapshot(
    st_reg_1 |> add_candidates(reg_res_sp, "reg_res_svm"),
    error = TRUE
  )

  expect_snapshot(
    st_0 |>
      add_candidates(reg_res_sp) |>
      add_candidates(reg_res_svm_2),
    error = TRUE
  )

  expect_snapshot(
    st_0 |>
      add_candidates(reg_res_sp) |>
      add_candidates(reg_res_svm_3),
    error = TRUE
  )

  st_reg_1_new_train <- st_reg_1
  attr(st_reg_1_new_train, "train") <- attr(st_reg_1, "train")[-1, ]
  expect_snapshot(
    error = TRUE,
    res <- st_reg_1_new_train |> add_candidates(reg_res_lr)
  )

  reg_res_lr_ <- lin_reg_spec <-
    parsnip::linear_reg() |>
    parsnip::set_engine("lm")

  reg_wf_lr <-
    workflows::workflow() |>
    workflows::add_model(lin_reg_spec) |>
    workflows::add_recipe(tree_frogs_reg_rec)

  set.seed(1)

  reg_res_lr_bad <-
    tune::fit_resamples(
      object = reg_wf_lr,
      resamples = reg_folds,
      metrics = yardstick::metric_set(yardstick::rmse),
      control = tune::control_resamples(
        save_pred = TRUE,
        save_workflow = FALSE
      )
    )

  expect_snapshot(
    stacks() |> add_candidates(reg_res_lr_bad),
    error = TRUE
  )

  suppressWarnings(
    reg_res_lr_bad2 <-
      tune::fit_resamples(
        object = reg_wf_lr,
        resamples = reg_folds,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = tune::control_resamples(
          save_pred = FALSE,
          save_workflow = TRUE
        )
      )
  )

  expect_snapshot(
    stacks() |> add_candidates(reg_res_lr_bad2),
    error = TRUE
  )

  reg_res_lr_renamed <- reg_res_lr

  expect_snapshot(
    stacks() |>
      add_candidates(reg_res_lr) |>
      add_candidates(reg_res_lr_renamed)
  )

  dat <-
    tibble::tibble(
      x = rnorm(200),
      y = x + rnorm(200, 0, .1),
      z = factor(sample(letters[1:3], 200, TRUE))
    )

  # use a metric that only relies on hard class prediction
  log_res <-
    tune::tune_grid(
      workflows::workflow() |>
        workflows::add_formula(z ~ x + y) |>
        workflows::add_model(
          parsnip::multinom_reg(
            penalty = tune::tune("penalty"),
            mixture = tune::tune("mixture")
          ) |>
            parsnip::set_engine("glmnet")
        ),
      rsample::vfold_cv(dat, v = 4),
      grid = 4,
      control = control_stack_grid(),
      metrics = yardstick::metric_set(yardstick::accuracy)
    )

  expect_snapshot(
    stacks() |> add_candidates(log_res),
    error = TRUE
  )

  # fake a censored regression tuning result
  wf <- attr(reg_res_lr, "workflow")
  wf$fit$actions$model$spec$mode <- "censored regression"
  attr(reg_res_lr, "workflow") <- wf

  expect_snapshot(
    stacks() |> add_candidates(reg_res_lr),
    error = TRUE
  )

  # warn when stacking may fail due to tuning failure
  # TODO: re-implement tests--devel tune now errors on previous failure
})

test_that("model definition naming works as expected", {
  skip_on_cran()

  st_reg_1_newname <-
    stacks() |>
    add_candidates(reg_res_svm, name = "boop")

  st_class_1_newname <-
    stacks() |>
    add_candidates(class_res_rf, name = "boop")

  st_log_1_newname <-
    stacks() |>
    add_candidates(log_res_rf, name = "boop")

  expect_true(ncol_with_name(st_reg_1, "reg_res_svm") > 0)
  expect_true(ncol_with_name(st_class_1, "class_res_rf") > 0)
  expect_true(ncol_with_name(st_log_1, "log_res_rf") > 0)

  expect_true(ncol_with_name(st_reg_1_newname, "boop") > 0)
  expect_true(ncol_with_name(st_class_1_newname, "boop") > 0)
  expect_true(ncol_with_name(st_log_1_newname, "boop") > 0)

  expect_equal(ncol_with_name(st_reg_1_newname, "reg_res_svm"), 0)
  expect_equal(ncol_with_name(st_class_1_newname, "class_res_rf"), 0)
  expect_equal(ncol_with_name(st_class_1_newname, "log_res_rf"), 0)

  expect_snapshot(
    st_reg_1 |>
      add_candidates(reg_res_sp, "reg_res_svm"),
    error = TRUE
  )

  expect_snapshot(
    st_class_1 |>
      add_candidates(class_res_nn, "class_res_rf"),
    error = TRUE
  )

  expect_snapshot(
    st_log_1 |>
      add_candidates(log_res_nn, "log_res_rf"),
    error = TRUE
  )

  expect_snapshot(
    st_reg_1 <-
      stacks() |>
      add_candidates(reg_res_svm, name = "beep bop")
  )
})

test_that("stacks can handle columns and levels named 'class'", {
  # waiting on https://github.com/tidymodels/tune/issues/487
  # to be able to test with entry "class"
  x <- tibble::tibble(
    class = sample(c("class_1", "class_2"), 100, replace = TRUE),
    a = rnorm(100),
    b = rnorm(100)
  )

  res <- tune::tune_grid(
    parsnip::logistic_reg(
      engine = 'glmnet',
      penalty = tune::tune(),
      mixture = 1
    ),
    preprocessor = recipes::recipe(class ~ ., x),
    resamples = rsample::vfold_cv(x, 2),
    grid = 2,
    control = control_stack_grid()
  )

  expect_s3_class(
    suppressWarnings(
      stacks() |>
        add_candidates(res)
    ),
    "data_stack"
  )
})

test_that("stacks can add candidates via workflow sets", {
  skip_on_cran()
  skip_if_not_installed("workflowsets")

  wf_set <-
    workflowsets::workflow_set(
      preproc = list(
        reg = tree_frogs_reg_rec,
        reg2 = tree_frogs_reg_rec,
        sp = spline_rec
      ),
      models = list(lr = lin_reg_spec, svm = svm_spec, lr2 = lin_reg_spec),
      cross = FALSE
    )

  wf_set_trained <-
    workflowsets::workflow_map(
      wf_set,
      fn = "tune_grid",
      seed = 1,
      resamples = reg_folds,
      metrics = yardstick::metric_set(yardstick::rmse),
      control = control_stack_grid()
    )

  set.seed(1)
  wf_set_stack <-
    stacks() |>
    add_candidates(wf_set_trained) |>
    blend_predictions() |>
    fit_members()

  set.seed(1)
  wf_stack <-
    stacks() |>
    add_candidates(
      wf_set_trained$result[[1]],
      name = wf_set_trained$wflow_id[[1]]
    ) |>
    add_candidates(
      wf_set_trained$result[[2]],
      name = wf_set_trained$wflow_id[[2]]
    ) |>
    add_candidates(
      wf_set_trained$result[[3]],
      name = wf_set_trained$wflow_id[[3]]
    ) |>
    blend_predictions() |>
    fit_members()

  expect_equal(
    predict(wf_set_stack, tree_frogs),
    predict(wf_stack, tree_frogs)
  )

  wf_set_trained_error <- wf_set_trained
  wf_set_trained_error$result[[1]] <- "boop"

  # check that warning is supplied and looks as it ought to
  expect_warning(
    wf_set_stack_2 <- stacks() |> add_candidates(wf_set_trained_error),
    class = "wf_set_partial_fit"
  )

  expect_snapshot(
    res <- stacks() |> add_candidates(wf_set_trained_error)
  )

  wf_set_trained_error$result[[2]] <- "boop"

  expect_snapshot(
    res <- stacks() |> add_candidates(wf_set_trained_error)
  )

  # now, will all resampled fits failing, should error
  wf_set_trained_error$result[[3]] <- "boop"

  expect_error(
    stacks() |> add_candidates(wf_set_trained_error),
    class = "wf_set_unfitted"
  )

  expect_snapshot(
    error = TRUE,
    stacks() |> add_candidates(wf_set_trained_error)
  )

  # check that add_candidate adds the candidates it said it would
  wf_stack_2 <-
    stacks() |>
    add_candidates(
      wf_set_trained$result[[2]],
      name = wf_set_trained$wflow_id[[2]]
    ) |>
    add_candidates(
      wf_set_trained$result[[3]],
      name = wf_set_trained$wflow_id[[3]]
    ) |>
    blend_predictions() |>
    fit_members()

  expect_equal(
    predict(wf_set_stack, tree_frogs),
    predict(wf_stack, tree_frogs)
  )
})
