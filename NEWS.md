# stacks

### v0.2.0

### Breaking changes

This release of the package changes some elements of the internal structure
of model stacks. As such, model stacks stored as saved objects will need to
be regenerated before predicting, plotting, printing, etc.

### New features

* The package now supports elastic net models as a meta-learner via 
  the `mixture` argument to `blend_predictions`.
* The package can now add candidates from `workflow_map` objects
  from the new {workflowsets} package. The interface to `add_candidates`
  for doing so is the same as with `tune_results` objects, and 
  `add_candidates` is now a generic function.
* Objects tuned with racing methods from the {finetune} package can now be
  added as candidate members.

### Bug fixes

* Fixed bug in determining member hyperparameters during member
  fitting when using non-RMSE/ROC AUC metrics.
* Fixed bug arising from  model definition names that are not valid column 
  names. The package will now message in the case that the provided names
  are not valid column names and use `make.names` for associated candidate
  members.  

### Miscellaneous improvements

* Drop {digest} dependency in favor of {tune}/{rsample} "fingerprinting"
  to check consistency of resamples.
* `fit_members()` will now warn when supplied a model stack whose
  members have already been fitted.
* Integrate with {tune} functionality for appropriately coloring errors, 
  warnings, and messages.
* Improved faceting and axis scales to make `autoplot` with `type = "members"`
  more informative.
* Various improvements to documentation.

### v0.1.0

Initial release!
