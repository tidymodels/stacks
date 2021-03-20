# stacks

### v0.1.0.9000

Developmental version, to be released as v0.2.0.

* Fixed bug in determining member hyperparameters during member
  fitting when using non-RMSE/ROC AUC metrics.
* Drop {digest} dependency in favor of {tune}/{rsample} "fingerprinting"
  to check consistency of resamples.
* `fit_members()` will now warn when supplied a model stack whose
  members have already been fitted.
* Introduce support for adding candidates from `workflow_map` objects
  from the new {workflowsets} package!
* Introduce support for adding candidates from objects tuned with
  racing methods.
* Integrate with {tune} functionality for appropriately coloring errors, 
  warnings, and messages.
* Fixed bug arising from  model definition names that are not valid column 
  names. The package will now message in the case that the provided names
  are not valid column names and use `make.names` for associated candidate members.
* Various bug fixes and improvements to documentation.

### v0.1.0

Initial release!
