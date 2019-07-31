# Updating to R 3.4.3

Our latest version of mobForest was focused on revamping existing code to work with the newest version of R. We also made strides to consolidate all code to common formats. Although no substantive changes were made (no new functions or changes to algorithms), users should be aware of these smaller tweaks. 

* All mobForest functions now use the `period.sep` naming convention
* All mobForest specific parameters now use the `underscore_sep` naming convention
* Unit tests were written for many of the basic functions. Although several tests are not ran using `testthat`. These tests were causing `R CMD Check` to run indefinitely. Forums suggested that this happens when using the parallel package. All tests passed on the latest build when ran locally. 
* CRAN suggests not using `:::` when calling hidden functions of another package. To overcome this, we copied the necessary hidden packages from `party` into `utility.R`. 

# Updating to R 3.5.2

No changes.