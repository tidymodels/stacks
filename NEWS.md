# stacks (development version)

# stacks 1.1.1

* A re-release of stacks 1.1.0 to address a note to resubmit following a since-resolved check failure.

# stacks 1.1.0

* Model stack and data stack print methods no longer raise conditions to
  print to the console (#228).

* Added missing commas and addressed formatting issues throughout the vignettes and articles. Backticks for package names were removed and missing parentheses for functions were added (@Joscelinrocha, #218).

* Increased the minimum R version to R 4.1.

* Transitioned support for parallel processing fully to the 
  [future](https://www.futureverse.org/) framework. Parallelism backends
  registered with foreach will be ignored with a warning (#234).
  
# stacks 1.0.5

* Addressed inflation of butchered model stack object size after saving and
  reloading (#214).
  
* Fixed type-checking bug for `add_candidates(name)`.

# stacks 1.0.4

* Introduced support for parallel processing using the [future](https://www.futureverse.org/) framework. The stacks package previously supported parallelism with foreach, and users can use either framework for now. In a future release, stacks will begin the deprecation cycle for parallelism with foreach, so we encourage users to begin migrating their code now. See [the _Parallel Processing_ section in the tune package's "Optimizations" article](https://tune.tidymodels.org/articles/extras/optimizations.html#parallel-processing) to learn more (#866).

* Improved error message for unsupported model modes (#152).

# stacks 1.0.3

* Refine package alt text (#202).

* Update example objects, resolving deprecation warnings from recipes (#203).

* Fix bug in type checking for `blend_predictions(mixture)` (#204).

* Resolve package-level documentation aliasing notice from CRAN.

# stacks 1.0.2

* Added an `augment()` method for `model_stack` objects (#173).

* Converted all character variables in the `tree_frogs` example data to factor 
  and updated downstream example objects (#177).
  
* Fixed bug that resulted in errors when using model formulas with the 
  `"mgcv"` engine (#193).
  
* Made several optimizations to reduce evaluation time and memory allocation
  when stacking.
  
* Various bug fixes and improvements to documentation.

# stacks 1.0.1

* Removes an unneeded data import attribute from the `tree_frogs` example data 
  and its associated objects (#148).
  
* `blend_predictions()` doesn't error anymore if the `control` argument isn't a 
  `control_grid` object. As long as the object passed to `control` 
  include the same elements as `control_grid()` output, 
  `parsnip::condense_control()` will handle input (#149).
  
* Tightened integration with the workflowsets package (#161, #165).
    - Refined logic with adding candidates via workflowsets to allow for 
      partially trained workflow sets. In the case that a workflow set contains 
      some failed tuning results, stacks will inform the user that they will be 
      excluded from the data stack and only add the results that trained successfully.
    - Extended documentation related to the packages' interactions, including 
      a [new article](https://stacks.tidymodels.org/dev/articles/workflowsets.html) 
      on the package website.

* Revamped errors, warnings, and messages. Prompts now provide more thorough 
  context about where they arose, include more extensive references to 
  documentation, and are correctly pluralized (#150, #167).
  
* Various bug fixes and improvements to documentation.

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
