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
    pred_p |>
    dplyr::select(where(is.numeric)) |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_longer(
      dplyr::starts_with(".pred_"),
      names_to = "level",
      values_to = "prob"
    ) |>
    dplyr::mutate(
      level = gsub(".pred_", "", level)
    ) |>
    dplyr::group_by(
      row
    ) |>
    dplyr::summarize(
      max = max(prob),
      level = level[prob == max]
    )

  expect_true(all(hard_class_preds$level == pred_c$.pred_class))
})

test_that("predict method errors informatively", {
  skip_on_cran()

  expect_snapshot(error = TRUE, st_reg_1 |> predict(penguins_test))

  expect_snapshot(error = TRUE, st_reg_1_ |> predict(penguins_test))

  expect_snapshot(
    error = TRUE,
    st_reg_1__ |> predict(penguins_test, members = "for sure!")
  )

  expect_snapshot(
    error = TRUE,
    st_reg_1__ |> predict(penguins_test, opts = TRUE)
  )
})

test_that("augment method works (regression)", {
  skip_on_cran()

  aug_r <- augment(st_reg_1__, tree_frogs_reg_test)
  aug_r2 <- augment(st_reg_1__, tree_frogs_reg_test, members = TRUE)
  aug_r3 <- augment(st_reg_1__, tree_frogs_reg_test[, 1:4])
  aug_r4 <- augment(st_reg_1__, tree_frogs_reg_test[, 1:4], members = TRUE)

  expect_true(all(c(".pred", ".resid") %in% names(aug_r)))
  expect_equal(nrow(aug_r), nrow(tree_frogs_reg_test))
  expect_s3_class(aug_r, "tbl_df")
  expect_equal(class(aug_r$.pred), "numeric")

  expect_true(all(
    c(
      ".pred",
      ".resid",
      paste0(c(".pred_"), names(st_reg_1__$member_fits)),
      paste0(c(".resid_"), names(st_reg_1__$member_fits))
    ) %in%
      names(aug_r2)
  ))
  expect_equal(nrow(aug_r2), nrow(tree_frogs_reg_test))
  expect_s3_class(aug_r2, "tbl_df")
  expect_true(all(colnames(aug_r2 %in% names(st_reg_1__$member_fits))))

  expect_true(".pred" %in% names(aug_r3))
  expect_false(".resid" %in% names(aug_r3))
  expect_equal(nrow(aug_r3), nrow(tree_frogs_reg_test))
  expect_s3_class(aug_r3, "tbl_df")
  expect_equal(class(aug_r3$.pred), "numeric")

  expect_true(all(
    c(".pred", paste0(c(".pred_"), names(st_reg_1__$member_fits))) %in%
      names(aug_r4)
  ))
  expect_false(".resid" %in% names(aug_r4))
  expect_equal(nrow(aug_r4), nrow(tree_frogs_reg_test))
  expect_s3_class(aug_r4, "tbl_df")
  expect_true(all(colnames(aug_r4 %in% names(st_reg_1__$member_fits))))
})

test_that("augment method works (multinomial classification)", {
  skip_on_cran()

  aug_c <- augment(st_class_1__, tree_frogs_class_test)
  aug_c2 <- augment(st_class_1__, tree_frogs_class_test, members = TRUE)
  aug_c3 <- augment(st_class_1__, tree_frogs_class_test[, c(1, 3:5)])
  aug_c4 <- augment(st_class_1__, tree_frogs_class_test, type = "prob")
  aug_c5 <- augment(
    st_class_1__,
    tree_frogs_class_test,
    type = "prob",
    members = TRUE
  )
  aug_c6 <- augment(
    st_class_1__,
    tree_frogs_class_test[, c(1, 3:5)],
    type = "prob"
  )

  res <- list(aug_c, aug_c2, aug_c3, aug_c4, aug_c5, aug_c6)

  expect_true(all(purrr::map_lgl(
    res,
    ~ nrow(.x) == nrow(tree_frogs_class_test)
  )))
  expect_true(all(purrr::map_lgl(res[1:3], ~ ".pred_class" %in% names(.x))))
  expect_true(all(purrr::map_lgl(res[4:6], ~ ".pred_full" %in% names(.x))))

  expect_true(all(
    c(
      ".pred_class",
      paste0(c(".pred_class_"), names(st_class_1__$member_fits))
    ) %in%
      names(aug_c2)
  ))
  expect_true(all(
    c(
      ".pred_full",
      paste0(c(".pred_full_"), names(st_class_1__$member_fits))
    ) %in%
      names(aug_c5)
  ))

  # no .resid here, so output ought to be the same w and w/o outcome:
  expect_equal(aug_c |> dplyr::select(-reflex), aug_c3)
  expect_equal(aug_c4 |> dplyr::select(-reflex), aug_c6)
})

test_that("augment method works (binary classification)", {
  skip_on_cran()

  aug_l <- augment(st_log_1__, tree_frogs_class_test)
  aug_l2 <- augment(st_log_1__, tree_frogs_class_test, members = TRUE)
  aug_l3 <- augment(st_log_1__, tree_frogs_class_test[, c(1:4)])
  aug_l4 <- augment(st_log_1__, tree_frogs_class_test, type = "prob")
  aug_l5 <- augment(
    st_log_1__,
    tree_frogs_class_test,
    type = "prob",
    members = TRUE
  )
  aug_l6 <- augment(st_log_1__, tree_frogs_class_test[, c(1:4)], type = "prob")

  res <- list(aug_l, aug_l2, aug_l3, aug_l4, aug_l5, aug_l6)

  expect_true(all(purrr::map_lgl(
    res,
    ~ nrow(.x) == nrow(tree_frogs_class_test)
  )))
  expect_true(all(purrr::map_lgl(res[1:3], ~ ".pred_class" %in% names(.x))))
  expect_true(all(purrr::map_lgl(res[4:6], ~ ".pred_yes" %in% names(.x))))

  expect_true(all(
    c(
      ".pred_class",
      paste0(c(".pred_class_"), names(st_log_1__$member_fits))
    ) %in%
      names(aug_l2)
  ))
  expect_true(all(
    c(".pred_yes", paste0(c(".pred_yes_"), names(st_log_1__$member_fits))) %in%
      names(aug_l5)
  ))

  # no .resid here, so output ought to be the same w and w/o outcome:
  expect_equal(aug_l |> dplyr::select(-hatched), aug_l3)
  expect_equal(aug_l4 |> dplyr::select(-hatched), aug_l6)
})
