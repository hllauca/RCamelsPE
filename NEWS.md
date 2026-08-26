# RCamelsPE 1.0.1

## Changes

* `download_camels_pe()` now points to CAMELS-PE v1.0.1 on Zenodo
  (<doi:10.5281/zenodo.21195425>), which is freely accessible.
* `download_camels_pe()` now requires an explicit `path` argument
  (previously defaulted to the current working directory).
* Added a bundled example dataset (`inst/extdata/sample_camels_pe`) used
  by the vignette and package examples.
* Added `utils` to `Imports` (already used internally via
  `utils::download.file()`, `utils::unzip()`, and
  `utils::globalVariables()`).
* Fixed documentation inconsistencies: corrected the example variable
  name in `plot_attribute_map()` and the example file name in
  `read_timeseries()`.

# RCamelsPE 1.0.0

## New features

* Initial CRAN release.
* Functions to read CAMELS-PE metadata, attributes, time series, and geospatial data.
* Plotting utilities for catchments and hydroclimatic data.
* Support for by-catchment and global data access modes.
