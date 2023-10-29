# emdi 2.2.1
* Substitution of maptools and rgeos by sf due to depreciation of the former
* Substitution of ggplot2::fortify by ggplot2::geom_sf due to depreciation of the former

# emdi 2.2.0
* Extension of the ebp function to allow for population weights
* Extension of the ebp function to allow the aggregation of the estimates to different levels  
* a more flexible use of the custom_indicator agrument within the ebp function

# emdi 2.1.3
* Improved summary with clearer notation of R2
* Updated Area Level (FH) vignette
* Minor improvements in checking T/F in if clauses
* Increased dependency to R (>= 4.2.0) corresponding to the imported package MuMin

# emdi 2.1.2
* Improved messages
* Copyrights updated
* Bug fix in direct variance estimation (direct)
* Bug fix in level orderings (ebp)
* Changed last name in description

# emdi 2.1.1
* Fix to account for the changed behavior of as.vector() on data frames

# emdi 2.1.0
* Extension of the ebp function to allow informative sampling
* Additional data-driven transformations for the ebp 
* New additional vignette
* Example in write.ods fixed

# emdi 2.0.3
* Robustifying tests to comply with alternative implementations
* Updated example for step


# emdi 2.0.2
* Many S3-methods in the style of stats and nlme are implemented for the classes direct, ebp and fh
* Structure of S3-classes has been cleaned up
* The bootstrap parameter in the fh-function has been changed from a single number to a single number or a numeric vector with two elements to allow for separately controlling the number of bootstrap iterations for the MSE estimation and the computation of the bootstrap based information criteria
* Renaming the robustness constant in the fh function
* Minor fixes in the documentation
* Reducing the sizes of the data sets used for test that tests to decrease testing time

# emdi 2.0.1
* Robustifying tests to comply with alternative BLAS/LAPACK implementations
* Robustifying tests to work with r-oldrel
* Updated R version dependency

# emdi 2.0.0
* Area-level models newly added via function `fh`
* All methods for `emdi model` are extended for `emdi model fh`
* Step function for area-level models newly added
* Three new data sets `eusilcA_smpAgg`, `eusilcA_popAgg` and 
`eusilcA_prox` have been integrated
* New additional vignette
* Change of argument order (model and direct) in function `compare_plot`
* Minor bug fixes in argument checkings and message handling
* Updated R version dependency

# emdi 1.1.7

* Minor typos corrected
* Unit-Tests adjusted to the forthcoming R-Version 4.0

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