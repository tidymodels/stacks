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

# ------------------------------------------------------------------------------

test_that("performance plots", {
  skip_on_cran()

  # ----------------------------------------------------------------------------
  # One mixture

  p1 <- autoplot(st_reg_1__)
  expect_equal(
    names(p1$data),
    c(
      "penalty",
      "mixture",
      ".metric",
      ".estimator",
      "mean",
      "n",
      "std_err",
      ".config"
    )
  )
  expect_equal(names(p1$facet$params$facets), ".metric")
  expect_equal(names(p1$mapping), c("x", "y"))
  expect_equal(
    rlang::expr_text(p1$mapping$x),
    "~penalty"
  )
  expect_equal(
    rlang::expr_text(p1$mapping$y),
    "~mean"
  )

  # ----------------------------------------------------------------------------
  # Multiple mixtures

  p2 <- autoplot(st_class_1_mixed)

  expect_equal(
    names(p2$data),
    c(
      "penalty",
      "mixture",
      ".metric",
      ".estimator",
      "mean",
      "n",
      "std_err",
      ".config"
    )
  )
  expect_true(is.character(p2$data$mixture))
  expect_equal(names(p2$facet$params$facets), ".metric")
  expect_equal(names(p2$mapping), c("x", "y", "colour"))
  expect_equal(
    rlang::expr_text(p2$mapping$x),
    "~penalty"
  )
  expect_equal(
    rlang::expr_text(p2$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(p2$mapping$colour),
    "~mixture"
  )
})

# ------------------------------------------------------------------------------

test_that("weight plots", {
  skip_on_cran()

  # ----------------------------------------------------------------------------
  # non-negative

  p1 <- autoplot(st_reg_1__, "weights")
  expect_equal(
    names(p1$data),
    c("terms", "model", "weight", "member")
  )
  expect_equal(names(p1$mapping), c("x", "y", "fill"))
  expect_equal(
    rlang::expr_text(p1$mapping$x),
    "~weight"
  )
  expect_equal(
    rlang::expr_text(p1$mapping$y),
    "~format(member)"
  )
  expect_equal(
    rlang::expr_text(p1$mapping$fill),
    "~model"
  )

  # ----------------------------------------------------------------------------
  # pos and neg coeffs

  p2 <- autoplot(st_class_1_non_neg, "weights")

  expect_equal(
    names(p2$data),
    c("terms", "model", "weight", "class", "member")
  )
  expect_equal(names(p2$facet$params$facets), "class")
  expect_equal(names(p1$mapping), c("x", "y", "fill"))
  expect_equal(
    rlang::expr_text(p1$mapping$x),
    "~weight"
  )
  expect_equal(
    rlang::expr_text(p1$mapping$y),
    "~format(member)"
  )
  expect_equal(
    rlang::expr_text(p1$mapping$fill),
    "~model"
  )
})


# ------------------------------------------------------------------------------

test_that("member plots", {
  skip_on_cran()

  # ----------------------------------------------------------------------------
  # One mixture

  p1 <- autoplot(st_reg_1__, "members")
  expect_equal(
    names(p1$data),
    c("penalty", "mixture", ".config", "num_members", "mean", ".metric")
  )
  expect_equal(names(p1$facet$params$facets), ".metric")
  expect_equal(names(p1$mapping), c("x", "y"))
  expect_equal(
    rlang::expr_text(p1$mapping$x),
    "~num_members"
  )
  expect_equal(
    rlang::expr_text(p1$mapping$y),
    "~mean"
  )

  # ----------------------------------------------------------------------------
  # Multiple mixtures

  p2 <- autoplot(st_class_1_mixed, "members")

  expect_equal(
    names(p2$data),
    c("penalty", "mixture", ".config", "num_members", "mean", ".metric")
  )
  expect_true(is.character(p2$data$mixture))
  expect_equal(names(p2$facet$params$facets), ".metric")
  expect_equal(names(p2$mapping), c("x", "y", "colour"))
  expect_equal(
    rlang::expr_text(p2$mapping$x),
    "~num_members"
  )
  expect_equal(
    rlang::expr_text(p2$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(p2$mapping$colour),
    "~mixture"
  )
})
