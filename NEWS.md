# emdi 1.1.7
 
* Minor typos corrected
* Unit-Tests adjusted to the forthcomming R-Version 4.0

# emdi 1.1.6
 
* New and updated references.

# emdi 1.1.5
 
* Tests updated to deal with new random number generation in R
* Some spelling improved

# emdi 1.1.4
 
* Fixed Bug in summary: R2 calculation with MuMIn is now fully working
* Added feature: formula used in fixed is now preserved, even if passed to ebp as a variable

# emdi 1.1.3

* The function `compare_plot` now benefits from a legend in all plots.
* Changes to be compatible with the forthcoming version of the MuMIn package.


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