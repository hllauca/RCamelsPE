# Internal environment to store package options
.camelspe_env <- new.env(parent = emptyenv())

#' Set CAMELS-PE dataset path
#'
#' Defines the root directory of the local CAMELS-PE dataset. The function
#' validates that the provided directory exists and contains the required
#' CAMELS-PE folder structure.
#'
#' Expected folders are:
#'
#' \itemize{
#'   \item \code{01_metadata}
#'   \item \code{02_attributes}
#'   \item \code{03_timeseries}
#'   \item \code{04_geospatial}
#' }
#'
#' Once defined, the path is internally stored and automatically used by
#' functions such as \code{read_metadata()},
#' \code{read_attributes()}, \code{read_timeseries()},
#' \code{read_geospatial()}, and \code{read_dictionary()}.
#'
#' @param path Character string. Path to the CAMELS-PE root directory.
#'
#' @return Invisibly returns the normalized CAMELS-PE path.
#'
#' @examples
#' \dontrun{
#' # Define CAMELS-PE root directory
#' set_camels_path("D:/DATA/CAMELS-PE")
#'
#' # Check the stored path
#' get_camels_path()
#' }
#'
#' @export
set_camels_path <- function(path) {

  if (!dir.exists(path)) {
    stop(
      "The provided CAMELS-PE path does not exist: ",
      path,
      call. = FALSE
    )
  }

  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  required_dirs <- c(
    "01_metadata",
    "02_attributes",
    "03_timeseries",
    "04_geospatial"
  )

  missing_dirs <- required_dirs[
    !dir.exists(file.path(path, required_dirs))
  ]

  if (length(missing_dirs) > 0) {
    stop(
      "The following required CAMELS-PE folders are missing: ",
      paste(missing_dirs, collapse = ", "),
      call. = FALSE
    )
  }

  .camelspe_env$camels_path <- path

  invisible(path)
}

#' Get CAMELS-PE dataset path
#'
#' Returns the CAMELS-PE root directory previously defined with
#' \code{set_camels_path()}.
#'
#' @return Character string with the CAMELS-PE root directory.
#'
#' @examples
#' \dontrun{
#' # Define CAMELS-PE root directory
#' set_camels_path("D:/DATA/CAMELS-PE")
#'
#' # Retrieve the stored path
#' get_camels_path()
#' }
#'
#' @export
get_camels_path <- function() {

  path <- .camelspe_env$camels_path

  if (is.null(path)) {
    stop(
      "CAMELS-PE path has not been set. ",
      "Use set_camels_path('path/to/CAMELS-PE') first.",
      call. = FALSE
    )
  }

  path
}
