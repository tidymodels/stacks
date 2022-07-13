# data stack printing works

    Code
      stacks()
    Output
      # A data stack with 0 model definitions and 0 candidate members.

---

    Code
      st_reg_1
    Output
      # A data stack with 1 model definition and 5 candidate members:
      #   reg_res_svm: 5 model configurations
      # Outcome: latency (numeric)

---

    Code
      st_class_1
    Output
      # A data stack with 1 model definition and 9.66666666666667 candidate members:
      #   class_res_rf: 9.66666666666667 model configurations
      # Outcome: reflex (factor)

---

    Code
      st_log_1
    Output
      # A data stack with 1 model definition and 10 candidate members:
      #   log_res_rf: 10 model configurations
      # Outcome: hatched (factor)

# model stack printing works

    Code
      st_reg_1_
    Message
      -- A stacked ensemble model -------------------------------------
      
      Out of 5 possible candidate members, the ensemble retained 2.
      Penalty: 0.1.
      Mixture: 1.
      
      The 2 highest weighted members are:
    Output
      # A tibble: 2 x 3
        member          type    weight
        <chr>           <chr>    <dbl>
      1 reg_res_svm_1_3 svm_rbf  1.26 
      2 reg_res_svm_1_2 svm_rbf  0.135
    Message
      
      Members have not yet been fitted with `fit_members()`.

---

    Code
      st_class_1_
    Message
      -- A stacked ensemble model -------------------------------------
      
      Out of 19 possible candidate members, the ensemble retained 11.
      Penalty: 0.001.
      Mixture: 1.
      Across the 3 classes, there are an average of 3.67 coefficients per class.
      
      The 10 highest weighted member classes are:
    Output
      # A tibble: 10 x 4
         member                       type        weight class
         <chr>                        <chr>        <dbl> <chr>
       1 .pred_mid_class_res_rf_1_01  rand_forest  7.71  mid  
       2 .pred_full_class_res_rf_1_05 rand_forest  7.03  full 
       3 .pred_mid_class_res_rf_1_07  rand_forest  5.66  low  
       4 .pred_mid_class_res_rf_1_08  rand_forest  5.56  low  
       5 .pred_mid_class_res_rf_1_06  rand_forest  3.96  mid  
       6 .pred_full_class_res_rf_1_07 rand_forest  2.09  full 
       7 .pred_full_class_res_rf_1_01 rand_forest  0.919 full 
       8 .pred_full_class_res_rf_1_09 rand_forest  0.386 full 
       9 .pred_mid_class_res_rf_1_10  rand_forest  0.164 mid  
      10 .pred_full_class_res_rf_1_06 rand_forest  0.129 full 
    Message
      
      Members have not yet been fitted with `fit_members()`.

---

    Code
      st_log_1_
    Message
      -- A stacked ensemble model -------------------------------------
      
      Out of 10 possible candidate members, the ensemble retained 3.
      Penalty: 1e-06.
      Mixture: 1.
      
      The 3 highest weighted member classes are:
    Output
      # A tibble: 3 x 3
        member                    type        weight
        <chr>                     <chr>        <dbl>
      1 .pred_yes_log_res_rf_1_05 rand_forest  3.54 
      2 .pred_yes_log_res_rf_1_02 rand_forest  3.26 
      3 .pred_yes_log_res_rf_1_09 rand_forest  0.203
    Message
      
      Members have not yet been fitted with `fit_members()`.

---

    Code
      st_reg_1__
    Message
      -- A stacked ensemble model -------------------------------------
      
      Out of 5 possible candidate members, the ensemble retained 2.
      Penalty: 0.1.
      Mixture: 1.
      
      The 2 highest weighted members are:
    Output
      # A tibble: 2 x 3
        member          type    weight
        <chr>           <chr>    <dbl>
      1 reg_res_svm_1_3 svm_rbf  1.26 
      2 reg_res_svm_1_2 svm_rbf  0.135

---

    Code
      st_class_1__
    Message
      -- A stacked ensemble model -------------------------------------
      
      Out of 19 possible candidate members, the ensemble retained 11.
      Penalty: 0.001.
      Mixture: 1.
      Across the 3 classes, there are an average of 3.67 coefficients per class.
      
      The 10 highest weighted member classes are:
    Output
      # A tibble: 10 x 4
         member                       type        weight class
         <chr>                        <chr>        <dbl> <chr>
       1 .pred_mid_class_res_rf_1_01  rand_forest  7.71  mid  
       2 .pred_full_class_res_rf_1_05 rand_forest  7.03  full 
       3 .pred_mid_class_res_rf_1_07  rand_forest  5.66  low  
       4 .pred_mid_class_res_rf_1_08  rand_forest  5.56  low  
       5 .pred_mid_class_res_rf_1_06  rand_forest  3.96  mid  
       6 .pred_full_class_res_rf_1_07 rand_forest  2.09  full 
       7 .pred_full_class_res_rf_1_01 rand_forest  0.919 full 
       8 .pred_full_class_res_rf_1_09 rand_forest  0.386 full 
       9 .pred_mid_class_res_rf_1_10  rand_forest  0.164 mid  
      10 .pred_full_class_res_rf_1_06 rand_forest  0.129 full 

---

    Code
      st_log_1__
    Message
      -- A stacked ensemble model -------------------------------------
      
      Out of 10 possible candidate members, the ensemble retained 3.
      Penalty: 1e-06.
      Mixture: 1.
      
      The 3 highest weighted member classes are:
    Output
      # A tibble: 3 x 3
        member                    type        weight
        <chr>                     <chr>        <dbl>
      1 .pred_yes_log_res_rf_1_05 rand_forest  3.54 
      2 .pred_yes_log_res_rf_1_02 rand_forest  3.26 
      3 .pred_yes_log_res_rf_1_09 rand_forest  0.203

