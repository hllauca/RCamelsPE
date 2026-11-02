# RCamelsPE 1.1.0

## New features

* `read_land_cover()` reads the annual land-cover series (1985-2024) at the
  class or subclass level, in wide or long format.
* `read_land_cover_legend()` reads the land-cover legend (MapBiomas code,
  subclass and class).
* `plot_land_cover()` plots the annual land-cover series as stacked areas.
* `read_attributes(type = "hydrological_sim")` reads the hydrological
  signatures computed from simulated streamflow.

## Changes

* The package now works with CAMELS-PE v2.0 (<doi:10.5281/zenodo.23890227>).
  `download_camels_pe()` downloads v2.0 by default; versions 1.0.1 and 1.0
  remain available through the `version` argument.
* References to the dataset use the Zenodo concept DOI
  (<doi:10.5281/zenodo.20058778>), which resolves to the latest version.
* Documentation updated to the new CAMELS-PE variable names (e.g.
  `prec_pisco`, `prec_sd_pisco`, `pet_hs`, `flow_obs_flag`, `flow_sim_m3s`).
* `read_timeseries()` reads columns with fixed types, so the quality-control
  columns (`flow_obs_flag`, `flow_obs_qc_test`) are always read correctly,
  and only reads the columns requested in `vars`.

## Bug fixes

* The `gauge_id` filter of `read_timeseries(global = TRUE)` and
  `read_attributes()` returned all catchments.

# RCamelsPE 1.0.0

## New features

* Initial CRAN release.
* Functions to read CAMELS-PE metadata, attributes, time series, and geospatial data.
* Plotting utilities for catchments and hydroclimatic data.
* Support for by-catchment and global data access modes.
