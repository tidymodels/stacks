# stacks 1.0.0

stacks 1.0.0 is the first production release of the package. While this release
includes only a few minor bug fixes, it's accompanied by a white paper
recently published in the Journal of Open Source software. You can read
that paper [here](https://doi.org/10.21105/joss.04471)!

This release:

* Addresses re-introduction of a bug arising from outcome levels that are not 
  valid column names in the multinomial classification setting (#133). 
* Fixes bug where stacks will return incorrect predictions if an elastic net
  meta-learner is used, the `type` argument to `predict` is set to `"class"`, 
  and the outcome levels differ from alphabetical order.
* Transitions package internals from functions deprecated from the recipes package.

# stacks 0.2.4 (GitHub only)

This is a GitHub-only release and does not change package source code. This 
update includes a `data-raw/paper` subdirectory containing source for a 
contributed paper to the Journal of Open Source Software.

# stacks 0.2.3

* Addressed deprecation warning in `add_candidates` (#99).
* Improved clarity of warnings/errors related to failed hyperparameter 
  tuning and resample fitting (#110).
* Reduced model stack object size and fixed bug where object size of model stack 
  inflated drastically after saving to file (#116). Also, regenerated example objects 
  with this change--saved model objects may need to be regenerated in order to 
  interface with newer versions of the package.
* Introduced a `times` argument to `blend_predictions` that is passed on to
  `rsample::bootstraps` when fitting stacking coefficients. Reducing this
  argument from its default (`25`) greatly reduces the run time of 
  `blend_predictions` (#94).
* The package will now load packages necessary for model fitting at 
  `fit_members()`, if available, and fail informatively if not (#118).
* Fixed bug where meta-learner tuning would fail with outcome names and levels
  including the string `"class"` (#125).

# stacks 0.2.2

* Fixed errors arising from outcome levels that are not valid column 
  names in the multinomial classification setting. 
* Fixed `collect_parameters` failing to return stacking coefficients
  in the two-class classification setting.
* Regenerated example objects with updated {rsample} fingerprinting--saved 
  model objects may need to be regenerated in order to build stacks combining
  models generated before and after this update.

# stacks 0.2.1

* Updates for importing workflow sets that use the `add_variables()` 
  preprocessor. 
* Plot fixes for cases where coefficients are negative. 
* Performance and member plots now show the effect of multiple mixture values. 
* Package diagrams now have alt text.

# stacks 0.2.0

## Breaking changes

This release of the package changes some elements of the internal structure
of model stacks. As such, model stacks stored as saved objects will need to
be regenerated before predicting, plotting, printing, etc.

## New features

* The package now supports elastic net models as a meta-learner via 
  the `mixture` argument to `blend_predictions`.
* The package can now add candidates from `workflow_map` objects
  from the new {workflowsets} package. The interface to `add_candidates`
  for doing so is the same as with `tune_results` objects, and 
  `add_candidates` is now a generic function.
* Objects tuned with racing methods from the {finetune} package can now be
  added as candidate members.

## Bug fixes

* Fixed bug in determining member hyperparameters during member
  fitting when using non-RMSE/ROC AUC metrics.
* Fixed bug arising from  model definition names that are not valid column 
  names. The package will now message in the case that the provided names
  are not valid column names and use `make.names` for associated candidate
  members.  

## Miscellaneous improvements

* Drop {digest} dependency in favor of {tune}/{rsample} "fingerprinting"
  to check consistency of resamples.
* `fit_members()` will now warn when supplied a model stack whose
  members have already been fitted.
* Integrate with {tune} functionality for appropriately coloring errors, 
  warnings, and messages.
* Improved faceting and axis scales to make `autoplot` with `type = "members"`
  more informative.
* Various improvements to documentation.

# stacks 0.1.0

Initial release!
