# RCamelsPE

**RCamelsPE** is an R package to read, process, and visualize data from the **CAMELS-PE dataset** (Catchment Attributes and Meteorology for Large-sample Studies – Peru).

It provides a simple and efficient interface to work with large-sample hydrological datasets for both research and operational applications.

---

## ⚠️ Important

The package **does NOT include the CAMELS-PE dataset**.  
Data must be stored locally.

---

## Features

- Efficient reading of time series by catchment
- Access to metadata and attributes
- Integration with geospatial data (gauges and catchments)
- Time series visualization
- Spatial visualization of catchments and attributes
- Tidyverse-friendly workflow
- Designed for large-sample and national-scale hydrology

---

## Installation

```r
install.packages("devtools")
devtools::install_github("hllauca/RCamelsPE")

library(RCamelsPE)
```

---

## CAMELS-PE Dataset Structure

```text
CAMELS-PE/
├── 01_metadata/
│   ├── stations.csv
│   ├── variable_dictionary.csv
│   └── attribute_dictionary.csv
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
│       └── ...
│
└── 04_geospatial/
    ├── camels_pe_gauges.gpkg
    ├── camels_pe_catchments.gpkg
```

---

## Example

```r
library(RCamelsPE)

# Set dataset path
set_camels_path("D:/DATA/CAMELS-PE")

# ---------------------------
# Metadata
# ---------------------------
stations <- read_metadata()

# Select one catchment
gauge_id <- stations$gauge_id[1]

# ---------------------------
# Time series
# ---------------------------
ts <- read_timeseries(
  gauge_id = gauge_id,
  vars = c("date", "gauge_id", "prec", "flow_obs")
)

# Plot streamflow
plot_timeseries(ts, variable = "flow_obs")

# ---------------------------
# Attributes
# ---------------------------
topo <- read_attributes(
  type = "topographic",
  gauge_id = gauge_id
)

head(topo)

# ---------------------------
# Geospatial data
# ---------------------------
catchments <- read_geospatial("catchments")
gauges <- read_geospatial("gauges")

# Plot selected catchment
plot_catchments(
  catchments = catchments,
  gauges = gauges,
  gauge_id = gauge_id
)

# ---------------------------
# Attribute map
# ---------------------------
plot_attribute_map(
  catchments = catchments,
  attributes = topo,
  variable = "area",
  gauges = gauges
)
```

---

## Design Principles

- Efficient reading by catchment
- Consistent variable naming (CAMELS convention)
- No data included in the package
- Scalable to large datasets
- Built on tidyverse and sf ecosystem

---

## License

### Package (code)
GNU General Public License (GPL ≥ 2)

### Dataset (CAMELS-PE)
Creative Commons Attribution 4.0 (CC BY 4.0)

---

## Citation

If you use this package or dataset, please cite:
Journal / DOI pending

---

