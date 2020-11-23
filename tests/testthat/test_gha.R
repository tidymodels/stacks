test_that("can access github workspace", {
  expect_equal(1, Sys.getenv("GITHUB_WORKSPACE"))
  
  expect_equal(1, class(Sys.getenv("GITHUB_WORKSPACE")))
  
  expect_equal(1, Sys.getenv("GITHUB_REF"))
})
