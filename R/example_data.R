#' Example Objects
#'
#' This package provides some resampling objects and datasets for use in examples
#' and vignettes derived from a dataset giving measurements on 333 penguins near
#' Palmer Station, Antarctica.
#' 
#' `penguins_train` and `penguins_test` are training and testing datasets, which
#' are subsets of data retrieved from Allison Horst's `palmerpenguins` package.
#' 
#' `reg_res_lr`, `reg_res_svm`, and `reg_res_sp` contain regression tuning results
#' for a linear regression, support vector machine, and spline model, respectively, 
#' fitting \code{body_mass_g} in the \code{palmerpenguins::penguins} data 
#' using all of the other variables as predictors. 
#' 
#' `class_res_rf` and `class_res_nn`, contain multiclass classification tuning 
#' results for a random forest and neural network classification model, 
#' respectively, fitting \code{year} (as a factor) in the 
#' \code{palmerpenguins::penguins} 
#' data using all of the other variables as predictors.
#' 
#' `log_res_rf` and `log_res_nn`, contain binary classification tuning results
#' for a random forest and neural network classification model, respectively, 
#' fitting \code{sex} in the \code{palmerpenguins::penguins} data 
#' using all of the other variables as predictors.
#' 
#' The source code for generating these objects is given below.
#' 
#' @includeRmd man-roxygen/example_models.Rmd
#' 
#' @source 
#' Gorman KB, Williams TD, Fraser WR (2014) Ecological Sexual Dimorphism
#' and Environmental Variability within a Community of Antarctic Penguins
#' (_Genus Pygoscelis_). PLoS ONE 9(3): e90081.
#' \url{https://doi.org/10.1371/journal.pone.0090081}
#' 
#' Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer
#' Archipelago (Antarctica) penguin data. R package version 0.1.0.
#' \url{https://github.com/allisonhorst/palmerpenguins}
#' 
#' @name example_data
NULL

#' @rdname example_data
"reg_res_svm"
#' @rdname example_data
"reg_res_sp"
#' @rdname example_data
"reg_res_lr"
#' @rdname example_data
"class_res_nn"
#' @rdname example_data
"class_res_rf"
#' @rdname example_data
"log_res_nn"
#' @rdname example_data
"log_res_rf"

#' @name penguins_train
#' @docType data
#' @keywords datasets
#' @rdname example_data
NULL

#' @name penguins_test
#' @docType data
#' @keywords datasets
#' @rdname example_data
NULL
