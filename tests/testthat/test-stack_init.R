context("stack_init")

test_that("stack_init", {
  inherits(st_0, "stack")
  inherits(st_0, "list")
  expect_null(get_outcome(st_0))
  expect_false(is_evaluated(st_0))
})
