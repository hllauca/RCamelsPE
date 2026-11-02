sample_path <- system.file("extdata", "sample_camels_pe", package = "RCamelsPE")

test_that("attributes are read and filtered by gauge_id", {
  clim <- read_attributes("climatic", gauge_id = "PE_212900", path = sample_path)
  expect_equal(clim$gauge_id, "PE_212900")

  hyd_sim <- read_attributes("hydrological_sim", path = sample_path)
  expect_true("q_sim_years" %in% names(hyd_sim))

  attr_all <- read_attributes("all", path = sample_path)
  expect_false(any(grepl("\\.(x|y)$", names(attr_all))))
})

test_that("time series are read with fixed column types", {
  ts <- read_timeseries("PE_212900", path = sample_path)
  expect_s3_class(ts$date, "Date")
  expect_type(ts$flow_obs_flag, "integer")
  expect_type(ts$flow_obs_qc_test, "character")

  ts_global <- read_timeseries(
    "PE_212900",
    global = TRUE,
    vars = c("date", "gauge_id", "prec_pisco"),
    path = sample_path
  )
  expect_equal(unique(ts_global$gauge_id), "PE_212900")
  expect_named(ts_global, c("date", "gauge_id", "prec_pisco"))
})

test_that("annual land cover is read at class and subclass level", {
  lc <- read_land_cover("class", years = 2000:2024, path = sample_path)
  expect_equal(range(lc$year), c(2000L, 2024L))

  lc_long <- read_land_cover(
    "subclass",
    gauge_id = "PE_212900",
    format = "long",
    path = sample_path
  )
  expect_false(anyNA(lc_long$subclass))
  totals <- tapply(lc_long$perc, lc_long$year, sum)
  expect_true(all(abs(totals - 100) < 0.01))

  expect_s3_class(plot_land_cover(lc_long, min_perc = 1), "ggplot")
})
