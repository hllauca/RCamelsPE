test_that("download_camels_pe() validates its arguments without downloading", {
  expect_error(download_camels_pe(), "valid destination")
  expect_error(
    download_camels_pe(path = tempdir(), version = "3.0"),
    "Unsupported CAMELS-PE version"
  )
  expect_equal(formals(download_camels_pe)$version, "2.0")
})
