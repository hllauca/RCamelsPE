# Internal environment to store package options
.camelspe_env <- new.env(parent = emptyenv())

#' Set CAMELS-PE dataset path
#'
#' @param path Character. Path to the CAMELS-PE root directory.
#'
#' @return Invisibly returns the normalized path.
#' @export
set_camels_path <- function(path) {
  if (!dir.exists(path)) {
    stop("The provided CAMELS-PE path does not exist: ", path)
  }

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  required_dirs <- c(
    "01_metadata",
    "02_attributes",
    "03_timeseries",
    "04_geospatial"
  )

  missing_dirs <- required_dirs[!dir.exists(file.path(path, required_dirs))]

  if (length(missing_dirs) > 0) {
    stop(
      "The following required CAMELS-PE folders are missing: ",
      paste(missing_dirs, collapse = ", ")
    )
  }

  .camelspe_env$camels_path <- path

  invisible(path)
}

#' Get CAMELS-PE dataset path
#'
#' @return Character path to the CAMELS-PE root directory.
#' @export
get_camels_path <- function() {
  path <- .camelspe_env$camels_path

  if (is.null(path)) {
    stop(
      "CAMELS-PE path has not been set. ",
      "Use set_camels_path('path/to/CAMELS-PE') first."
    )
  }

  path
}
