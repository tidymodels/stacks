#' @section Example Data:
#' 
#' These examples make use of data and resampling objects exported with the
#' package. All of them are derived from test cases making use of the
#' `penguins` dataset from Allison Horst's `palmerpenguins` package. 
#' 
#' The `penguins_train` and `penguins_test` objects are subsets of the
#' penguins data for using in training and testing, respectively.
#' 
#' Objects containing the substring `_res_` are `tune_results` objects
#' for model specifications. The `reg` prefix indicates that the model
#' definition is for use in regression, `class` indicates multinomial
#' classification, and `log` (as in logistic) indicates binomial classification.
#' The suffix indicates the model definition type. All of these `tune_results`
#' objects are fitted on a 5-fold cross validation of the `penguins_train` data.
#' 
#' For the regression setting, these `tune_results` objects reflect
#' models specified to fit `body_mass_g` using all of the other variables
#' as predictors. These objects include:  
#' 
#' * `reg_res_lr`: Fitted resamples for a linear regression model
#' * `reg_res_sp`: Tuning results for a splines model
#' * `reg_res_svm`: Tuning results for a support vector machine model
#' 
#' In the multinomial classification setting, the relevant objects reflect 
#' models specified to fit `island` using all of the other variables as 
#' predictors. These objects include:
#' 
#' * `class_res_nn`: Fitted resamples for a neural network model
#' * `class_res_rf`: Tuning results for a random forest model
#' 
#' In the binomial classification setting, the relevant objects reflect models
#' specified to fit `sex` using all of the other variables as predictors. 
#' These objects include:
#' 
#' * `log_res_nn`: Fitted resamples for a neural network model
#' * `log_res_rf`: Tuning results for a random forest model
#' 
#' See \code{?example_data} to learn more about these objects, as well as browse
#' the source code that generated them.
