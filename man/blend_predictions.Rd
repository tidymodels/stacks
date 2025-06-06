% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blend_predictions.R
\name{blend_predictions}
\alias{blend_predictions}
\title{Determine stacking coefficients from a data stack}
\usage{
blend_predictions(
  data_stack,
  penalty = 10^(-6:-1),
  mixture = 1,
  non_negative = TRUE,
  metric = NULL,
  control = tune::control_grid(),
  times = 25,
  ...
)
}
\arguments{
\item{data_stack}{A \code{data_stack} object}

\item{penalty}{A numeric vector of proposed values for total amount of
regularization used in member weighting. Higher penalties will generally
result in fewer members being included in the resulting model stack, and
vice versa. The package will tune over a grid formed from the cross
product of the \code{penalty} and \code{mixture} arguments.}

\item{mixture}{A number between zero and one (inclusive) giving the
proportion of L1 regularization (i.e. lasso) in the model. \code{mixture = 1}
indicates a pure lasso model, \code{mixture = 0} indicates ridge regression, and
values in \verb{(0, 1)} indicate an elastic net. The package will tune over
a grid formed from the cross product of the \code{penalty} and \code{mixture}
arguments.}

\item{non_negative}{A logical giving whether to restrict stacking
coefficients to non-negative values. If \code{TRUE} (default), 0 is passed as
the \code{lower.limits} argument to \code{\link[glmnet:glmnet]{glmnet::glmnet()}} in fitting the
model on the data stack. Otherwise, \code{-Inf}.}

\item{metric}{A call to \code{\link[yardstick:metric_set]{yardstick::metric_set()}}. The metric(s) to use in
tuning the lasso penalty on the stacking coefficients. Default values are
determined by \code{\link[tune:tune_grid]{tune::tune_grid()}} from the outcome class.}

\item{control}{An object inheriting from \code{control_grid} to be passed to
the model determining stacking coefficients. See \code{\link[tune:control_grid]{tune::control_grid()}}
documentation for details on possible values. Note that any \code{extract}
entry will be overwritten internally.}

\item{times}{Number of bootstrap samples tuned over by the model that
determines stacking coefficients. See \code{\link[rsample:bootstraps]{rsample::bootstraps()}} to
learn more.}

\item{...}{Additional arguments. Currently ignored.}
}
\value{
A \code{model_stack} object—while \code{model_stack}s largely contain the
same elements as \code{data_stack}s, the primary data objects shift from the
assessment set predictions to the member models.
}
\description{
Evaluates a data stack by fitting a regularized model on the
assessment predictions from each candidate member to predict
the true outcome.

This process determines the "stacking coefficients" of the model
stack. The stacking coefficients are used to weight the
predictions from each candidate (represented by a unique column
in the data stack), and are given by the betas of a LASSO model
fitting the true outcome with the predictions given in the
remaining columns of the data stack.

Candidates with non-zero stacking coefficients are model stack
members, and need to be trained on the full training set (rather
than just the assessment set) with \code{\link[=fit_members]{fit_members()}}. This function
is typically used after a number of calls to \code{\link[=add_candidates]{add_candidates()}}.
}
\details{
Note that a regularized linear model is one of many possible
learning algorithms that could be used to fit a stacked ensemble
model. For implementations of additional ensemble learning algorithms, see
\code{\link[h2o:h2o.stackedEnsemble]{h2o::h2o.stackedEnsemble()}} and \code{SuperLearner::SuperLearner()}.
}
\section{Example Data}{


This package provides some resampling objects and datasets for use in examples
and vignettes derived from a study on 1212 red-eyed tree frog embryos!

Red-eyed tree frog (RETF) embryos can hatch earlier than their normal
7ish days if they detect potential predator threat. Researchers wanted
to determine how, and when, these tree frog embryos were able to detect
stimulus from their environment. To do so, they subjected the embryos
at varying developmental stages to "predator stimulus" by jiggling
the embryos with a blunt probe. Beforehand, though some of the embryos
were treated with gentamicin, a compound that knocks out their lateral
line (a sensory organ.) Researcher Julie Jung and her crew found that
these factors inform whether an embryo hatches prematurely or not!

Note that the data included with the stacks package is not necessarily
a representative or unbiased subset of the complete dataset, and is
only for demonstrative purposes.

\code{reg_folds} and \code{class_folds} are \code{rset} cross-fold validation objects
from \code{rsample}, splitting the training data into for the regression
and classification model objects, respectively. \code{tree_frogs_reg_test} and
\code{tree_frogs_class_test} are the analogous testing sets.

\code{reg_res_lr}, \code{reg_res_svm}, and \code{reg_res_sp} contain regression tuning results
for a linear regression, support vector machine, and spline model, respectively,
fitting \code{latency} (i.e. how long the embryos took to hatch in response
to the jiggle) in the \code{tree_frogs} data, using most all of the other
variables as predictors. Note that the data underlying these models is
filtered to include data only from embryos that hatched in response to
the stimulus.

\code{class_res_rf} and \code{class_res_nn} contain multiclass classification tuning
results for a random forest and neural network classification model,
respectively, fitting \code{reflex} (a measure of ear function) in the
data using most all of the other variables as predictors.

\code{log_res_rf} and \code{log_res_nn}, contain binary classification tuning results
for a random forest and neural network classification model, respectively,
fitting \code{hatched} (whether or not the embryos hatched in response
to the stimulus) using most all of the other variables as predictors.

See \code{?example_data} to learn more about these objects, as well as browse
the source code that generated them.
}

\examples{
\dontshow{if ((stacks:::should_run_examples(suggests = "kernlab"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# see the "Example Data" section above for
# clarification on the objects used in these examples!

# put together a data stack
reg_st <-
  stacks() |>
  add_candidates(reg_res_lr) |>
  add_candidates(reg_res_svm) |>
  add_candidates(reg_res_sp)

reg_st

# evaluate the data stack
reg_st |>
  blend_predictions()

# include fewer models by proposing higher penalties
reg_st |>
  blend_predictions(penalty = c(.5, 1))

# allow for negative stacking coefficients
# with the non_negative argument
reg_st |>
  blend_predictions(non_negative = FALSE)

# use a custom metric in tuning the lasso penalty
library(yardstick)
reg_st |>
  blend_predictions(metric = metric_set(rmse))

# pass control options for stack blending
reg_st |>
  blend_predictions(
    control = tune::control_grid(allow_par = TRUE)
  )

# to speed up the stacking process for preliminary
# results, bump down the `times` argument:
reg_st |>
  blend_predictions(times = 5)

# the process looks the same with
# multinomial classification models
class_st <-
  stacks() |>
  add_candidates(class_res_nn) |>
  add_candidates(class_res_rf) |>
  blend_predictions()

class_st

# ...or binomial classification models
log_st <-
  stacks() |>
  add_candidates(log_res_nn) |>
  add_candidates(log_res_rf) |>
  blend_predictions()

log_st
\dontshow{\}) # examplesIf}
}
\seealso{
Other core verbs: 
\code{\link{add_candidates}()},
\code{\link{fit_members}()},
\code{\link{stacks}()}
}
\concept{core verbs}
