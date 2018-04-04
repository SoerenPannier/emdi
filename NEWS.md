# emdi 1.1.2

* The function `compare_plot` has been added to allow for an easy comparison 
between direct and model based estimates.
* Argument checks have been added, and improved
* Additional example in `map_plot` in order to explain the mapping table
* The datasets have been improved to allow for more realistic examples
* Small bug fixes
* Updated Vignette

# emdi 1.1.1

* The function `ebp` benefits from a new parameter called `seed` that allows 
reproducibility even when the function is run in parallel mode.
* Argument checks have been added, and improved
* Additional example in `map_plot` and `direct` 
* Updated Vignette

# emdi 1.1.0
  
* A new function `direct` is made available, which provides direct estimation for small areas.
* The function `ebp` now allows for a user-defined threshold.
* The function `ebp` is now able to perform a semi-parametric wild bootstrap for MSE estimation.
* The function `ebp` has new default value for parallelization that automatically adopts for the operating system.
* The two data sets `eusilcA_smp` and `eusilcA_pop` have been updated.
* For the function `map_plot` additional customization is now applicable.
* All methods for `emdi model` except plot are extended for `emdi direct`.
* `subset` and `as.data.frame` have been added as methods for class `emdi.estimators`