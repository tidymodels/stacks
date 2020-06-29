# Some objects for use throughout unit tests.
context("helpers")

# Stacks
# ------------------------------------------------------------------------
# st_0 <- stacks()
# 
# st_1 <- stacks() %>%
#   add_members(svm_res_)
# 
# st_0_rm <- st_1 %>%
#   remove_members("svm_res_")
# 
# st_2 <- stacks() %>%
#   add_members(svm_res_) %>%
#   add_members(spline_res_)
# 
# st_1_rm <- st_2 %>%
#   remove_members("spline_res_")
  
# Resampling Objects
# ------------------------------------------------------------------------

# generate a resampling object with a slightly different set of folds
set.seed(2)

