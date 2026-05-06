#' Read CAMELS-PE Station Metadata
#'
#' Reads the station metadata table from the CAMELS-PE dataset. This file
#' contains the main information for each gauging station and catchment,
#' including identifiers, station names, geographic coordinates,
#' hydrographic regions, observation periods, nested catchment information,
#' and other available metadata fields depending on the CAMELS-PE dataset
#' version.
#'
#' The function reads the file:
#'
#' \code{01_metadata/stations.csv}
#'
#' from the CAMELS-PE root directory.
#'
#' Use \code{read_dictionary()} to inspect available metadata fields,
#' descriptions, units, and data sources.
#'
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, the path previously defined with
#'   \code{set_camels_path()} is used.
#'
#' @return A tibble with CAMELS-PE station metadata.
#'
#' @examples
#' \dontrun{
#' # Define the CAMELS-PE dataset path
#' set_camels_path("path/to/CAMELS-PE")
#'
#' # Inspect available metadata variables
#' read_dictionary(category = "metadata")
#'
#' # Read station metadata
#' stations <- read_metadata()
#'
#' # Preview the first rows
#' head(stations)
#' }
#'
#' @export
read_metadata <- function(path = get_camels_path()) {

  file <- file.path(path, "01_metadata", "stations.csv")

  if (!file.exists(file)) {
    stop("The metadata file 'stations.csv' was not found in '01_metadata'.")
  }

  data <- readr::read_csv(
    file,
    show_col_types = FALSE
  )

  if (!"gauge_id" %in% names(data)) {
    stop("The metadata file must contain a 'gauge_id' column.")
  }

  data
}
