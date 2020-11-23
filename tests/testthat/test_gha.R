test_that("can access github workspace", {
  expect_equal("", Sys.getenv("GITHUB_WORKSPACE"))
  
  expect_equal("", class(Sys.getenv("GITHUB_WORKSPACE")))
  
  expect_equal("", Sys.getenv("GITHUB_REF"))
})
