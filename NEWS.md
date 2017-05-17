# emdi 1.1.0
  
* A new function `direct` is made available, which provides direct estimation for small areas.
* The function `ebp` now allows for a user-defined threshold.
* The function `ebp` is now able to perform a semi-parametric wild bootstrap for MSE estimation.
* The function `ebp` has new default value for parallelization that automatically adopts for the operating system.
* The two data sets `eusilcA_smp` and `eusilcA_pop` have been updated.
* For the function `map_plot` additional customization is now applicable.
* All methods for `emdi model` except plot are extended for `emdi direct`.
* `subset` and `as.data.frame` have been added as methods for class `emdi.estimators`