sample_path <- system.file(
  "extdata",
  "sample_camels_pe",
  package = "RCamelsPE"
)

test_that("set_camels_path/get_camels_path store the sample dataset path", {

  set_camels_path(sample_path)

  expect_equal(
    normalizePath(get_camels_path(), winslash = "/"),
    normalizePath(sample_path, winslash = "/")
  )
})

test_that("read_metadata reads the sample stations table", {

  stations <- read_metadata(path = sample_path)

  expect_true("gauge_id" %in% names(stations))
  expect_gt(nrow(stations), 0)
})

test_that("read_dictionary reads and filters the sample dictionary", {

  dict <- read_dictionary(path = sample_path)

  expect_true(all(c("category", "variable", "file") %in% names(dict)))
  expect_gt(nrow(dict), 0)

  topo_dict <- read_dictionary(category = "topographic", path = sample_path)

  expect_true(all(topo_dict$category == "topographic"))
})

test_that("read_attributes reads a single group and joins all groups", {

  topo <- read_attributes(type = "topographic", path = sample_path)

  expect_true("gauge_id" %in% names(topo))
  expect_true("area" %in% names(topo))

  attr_all <- read_attributes(type = "all", path = sample_path)

  expect_true("gauge_id" %in% names(attr_all))
  expect_gte(ncol(attr_all), ncol(topo))
})

test_that("read_timeseries reads by-catchment and global modes", {

  ts_catchment <- read_timeseries(
    gauge_id = "PE_212900",
    vars = c("date", "gauge_id", "prec", "flow_obs"),
    path = sample_path
  )

  expect_true(all(c("date", "gauge_id", "prec", "flow_obs") %in% names(ts_catchment)))
  expect_true(inherits(ts_catchment$date, "Date"))
  expect_true(all(ts_catchment$gauge_id == "PE_212900"))

  ts_global <- read_timeseries(global = TRUE, path = sample_path)

  expect_true("gauge_id" %in% names(ts_global))
  expect_gt(nrow(ts_global), 0)
})

test_that("read_geospatial reads gauges and catchments", {

  gauges <- read_geospatial(type = "gauges", path = sample_path)
  catchments <- read_geospatial(type = "catchments", path = sample_path)

  expect_true(inherits(gauges, "sf"))
  expect_true(inherits(catchments, "sf"))
  expect_true("gauge_id" %in% names(gauges))
  expect_true("gauge_id" %in% names(catchments))
})
