# Some objects for use throughout unit tests.
context("helpers")

# Stacks
# ------------------------------------------------------------------------
# st_0 <- stacks()
# 
# st_1 <- stacks() %>%
#   stack_resamples(svm_res_)
# 
# st_0_rm <- st_1 %>%
#   remove_members("svm_res_")
# 
# st_2 <- stacks() %>%
#   stack_resamples(svm_res_) %>%
#   stack_resamples(spline_res_)
# 
# st_1_rm <- st_2 %>%
#   remove_members("spline_res_")
  
# Resampling Objects
# ------------------------------------------------------------------------

# generate a resampling object with a slightly different set of folds
set.seed(2)

