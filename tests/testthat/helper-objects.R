# Some objects for use throughout unit tests.
context("helpers")

# Stacks
# ------------------------------------------------------------------------
# st_0 <- stacks()
# 
# st_1 <- stacks() %>%
#   add_candidates(reg_res_svm)
# 
# st_0_rm <- st_1 %>%
#   remove_members("reg_res_svm")
# 
# st_2 <- stacks() %>%
#   add_candidates(reg_res_svm) %>%
#   add_candidates(reg_res_sp)
# 
# st_1_rm <- st_2 %>%
#   remove_members("reg_res_sp")
  
# Resampling Objects
# ------------------------------------------------------------------------

# generate a resampling object with a slightly different set of folds
set.seed(2)

