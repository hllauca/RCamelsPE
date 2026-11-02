## Test environments

* local Windows 11, R 4.5.1 (R CMD check --as-cran, including PDF manual and vignettes)
* win-builder (release and devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Comments

The package provides tools to access, process, visualize, and download
the CAMELS-PE dataset for Peru.

The dataset itself is distributed separately through Zenodo and is not
included in the package. A small sample of the dataset (two catchments)
is included in inst/extdata so that examples, tests and the vignette run
without downloading the full dataset.
