---
editor_options: 
  markdown: 
    wrap: 72
---

# RCamelsPE

`RCamelsPE` is an R package to read, process, and visualize data from
the CAMELS-PE dataset (Catchment Attributes and Meteorology for
Large-sample Studies - Peru).

The package provides a simple and efficient interface to work with
large-sample hydrological datasets for research and operational
applications.

------------------------------------------------------------------------

## Important

The package does not include the CAMELS-PE dataset.

Dataset files must be downloaded separately and stored locally before
using the package.

------------------------------------------------------------------------

## Features

-   Efficient reading of time series by catchment
-   Access to metadata and catchment attributes
-   Access to data dictionaries and variable descriptions
-   Integration with geospatial data (gauges and catchments)
-   Time series visualization
-   Spatial visualization of catchments and attributes
-   Tidyverse-friendly workflow
-   Designed for large-sample and national-scale hydrology

------------------------------------------------------------------------

## Installation

``` r
install.packages("remotes")

remotes::install_github("hllauca/RCamelsPE")

library(RCamelsPE)
```

------------------------------------------------------------------------

## Documentation

-   Package website: <https://hllauca.github.io/RCamelsPE/>

-   Tutorial vignette:
    <https://hllauca.github.io/RCamelsPE/articles/rcamelspe.html>

------------------------------------------------------------------------

## CAMELS-PE Dataset Structure

``` text
CAMELS-PE/
│
├── 01_metadata/
│   ├── stations.csv
│   └── data_dictionary.csv
│
├── 02_attributes/
│   ├── topographic_attributes.csv
│   ├── climatic_indices.csv
│   ├── hydrological_signatures.csv
│   ├── landcover_attributes.csv
│   ├── geologic_attributes.csv
│   ├── soil_attributes.csv
│   └── human_intervention_attributes.csv
│
├── 03_timeseries/
│   ├── timeseries.csv
│   └── by_catchment/
│       ├── PE_XXXX.csv
│       ├── PE_XXXX.csv
│       └── ...
│
└── 04_geospatial/
    ├── camels_pe_gauges.gpkg
    ├── camels_pe_catchments.gpkg
    └── by_catchment/
        ├── PE_XXXX/
        │   ├── PE_XXXX_outlet.gpkg
        │   └── PE_XXXX_catchment.gpkg
        ├── PE_XXXX/
        │   ├── PE_XXXX_outlet.gpkg
        │   └── PE_XXXX_catchment.gpkg
        └── ...
```

------------------------------------------------------------------------

## Example

``` r
library(RCamelsPE)

# Define CAMELS-PE path
set_camels_path("path/to/CAMELS-PE")

# Inspect available variables
read_dictionary(category = "timeseries")

# Read metadata
stations <- read_metadata()

# Read time series
ts <- read_timeseries(
  gauge_id = "PE_221804",
  vars = c("date", "flow_obs", "prec")
)

# Plot streamflow
plot_timeseries(ts, variable = "flow_obs")
```

------------------------------------------------------------------------

## Design Principles

-   Efficient reading by catchment
-   Consistent variable naming following CAMELS conventions
-   No data included in the package
-   Scalable to large datasets
-   Built on the tidyverse and sf ecosystem

------------------------------------------------------------------------

## License

### Package code

GNU General Public License (GPL \>= 2)

### CAMELS-PE dataset

Creative Commons Attribution 4.0 (CC BY 4.0)

------------------------------------------------------------------------

## Citation

If you use CAMELS-PE or the RCamelsPE package, please cite:

### CAMELS-PE dataset

Llauca, H., Montesinos-Caceres, C., Gutierrez-Reynaga, M., &
Lavado-Casimiro, W. (2026). *CAMELS-PE: Catchment Attributes and
Meteorology for Large-sample Studies in Peru* (Version 1.0) [Dataset].
Zenodo. <https://doi.org/10.5281/zenodo.20058778>

### RCamelsPE package

Llauca, H. (2026). *RCamelsPE: R package for the CAMELS-PE hydrological
dataset*. GitHub repository. <https://github.com/hllauca/RCamelsPE>

### Article

Llauca, H., Montesinos-Caceres, C., Gutierrez-Reynaga, M., &
Lavado-Casimiro, W. (2026). *CAMELS-PE: Hydrometeorological time series
and catchment attributes for 136 catchments in Peru*. Earth System
Science Data, in preparation.
