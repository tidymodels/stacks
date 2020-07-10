test_that("data stack printing works", {
  expect_output(
    print(st_reg_1), 
    "# A data stack with 1 model definition and 5 candidate members:\\n#   reg_res_svm: 5 sub-models"
  )
  
  expect_output(
    print(st_reg_1_), 
    "# An unfitted model stack with 5 candidate members from 1 model definition."
  )
  
  expect_output(
    print(st_reg_1__), 
    "# A fitted model stack with 2 members:\\n#   reg_res_svm3, reg_res_svm2"
  )
})
