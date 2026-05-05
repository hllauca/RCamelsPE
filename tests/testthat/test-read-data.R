test_that("metadata can be read when CAMELS-PE path exists", {

  path <- Sys.getenv("CAMELSPE_PATH")

  skip_if(path == "")
  skip_if_not(dir.exists(path))

  set_camels_path(path)

  stations <- read_metadata()

  expect_true("gauge_id" %in% names(stations))
  expect_gt(nrow(stations), 0)
})
