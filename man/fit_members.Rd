% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_members.R
\name{fit_members}
\alias{fit_members}
\title{Fit model stack members}
\usage{
fit_members(model_stack, verbose = FALSE, ...)
}
\arguments{
\item{model_stack}{A \code{model_stack} object outputted by \code{blend_predictions()} or
\code{fit_members()}}

\item{verbose}{A logical for logging results as they are generated. Despite
this argument, warnings and errors are always shown.}

\item{...}{Additional arguments. Currently ignored.}
}
\value{
A \code{model_stack} object with a subclass inherited from the chosen
\verb{*_stack} method---this fitted model contains the
necessary components to predict on new data.
}
\description{
Fit model stack members
}
\section{Example Data}{


These examples make use of data and resampling objects exported with the
package. All of them are derived from test cases making use of the
\code{penguins} dataset from Allison Horst's \code{palmerpenguins} package.

The \code{penguins_train} and \code{penguins_test} objects are subsets of the
penguins data for using in training and testing, respectively.

Objects containing the substring \verb{_res_} are \code{tune_results} objects
for model specifications. The \code{reg} prefix indicates that the model
definition is for use in regression, \code{class} indicates multinomial
classification, and \code{log} (as in logistic) indicates binomial classification.
The suffix indicates the model definition type. All of these \code{tune_results}
objects are fitted on a 5-fold cross validation of the \code{penguins_train} data.

For the regression setting, these \code{tune_results} objects reflect
models specified to fit \code{body_mass_g} using all of the other variables
as predictors. These objects include:
\itemize{
\item \code{reg_res_lr}: Fitted resamples for a linear regression model
\item \code{reg_res_sp}: Tuning results for a splines model
\item \code{reg_res_svm}: Tuning results for a support vector machine model
}

In the multinomial classification setting, the relevant objects reflect
models specified to fit \code{year} (as a factor) using all of the other variables
as predictors. These objects include:
\itemize{
\item \code{class_res_nn}: Fitted resamples for a neural network model
\item \code{class_res_rf}: Tuning results for a random forest model
}

In the binomial classification setting, the relevant objects reflect models
specified to fit \code{sex} using all of the other variables as predictors.
These objects include:
\itemize{
\item \code{log_res_nn}: Fitted resamples for a neural network model
\item \code{log_res_rf}: Tuning results for a random forest model
}

See \code{?example_data} to learn more about these objects, as well as browse
the source code that generated them.
}

\examples{
\donttest{
# see the "Example Data" section above for
# clarification on the objects used in these examples!

# put together a data stack
reg_st <- 
  stacks() \%>\%
  add_candidates(reg_res_lr) \%>\%
  add_candidates(reg_res_svm) \%>\%
  add_candidates(reg_res_sp)
  
reg_st

# evaluate the data stack and fit the member models
reg_st \%>\%
  blend_predictions() \%>\%
  fit_members()
  
reg_st
  
# do the same with multinomial classification models
class_st <-
  stacks() \%>\%
  add_candidates(class_res_nn) \%>\%
  add_candidates(class_res_rf) \%>\%
  blend_predictions() \%>\%
  fit_members()
  
class_st
  
# ...or binomial classification models
log_st <-
  stacks() \%>\%
  add_candidates(log_res_nn) \%>\%
  add_candidates(log_res_rf) \%>\%
  blend_predictions() \%>\%
  fit_members()
  
log_st
}

}
\seealso{
Other core verbs: 
\code{\link{add_candidates}()},
\code{\link{blend_predictions}()},
\code{\link{stacks}()}
}
\concept{core verbs}