#' Read CAMELS-PE Geospatial Data
#'
#' Reads geospatial data from the CAMELS-PE dataset, including gauge locations
#' and catchment boundaries. The data are stored as GeoPackage files in the
#' \code{04_geospatial} directory.
#'
#' Available types:
#'
#' \itemize{
#'   \item \code{"gauges"}: point locations of gauging stations.
#'   \item \code{"catchments"}: polygon boundaries of catchments.
#' }
#'
#' @param type Character string. Either \code{"gauges"} or \code{"catchments"}.
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, the path previously defined with
#'   \code{set_camels_path()} is used.
#'
#' @return An \code{sf} object.
#'
#' @examples
#' \dontrun{
#' set_camels_path("D:/DATA/CAMELS-PE")
#'
#' # Read gauges
#' gauges <- read_geospatial("gauges")
#'
#' # Read catchments
#' catchments <- read_geospatial("catchments")
#'
#' # Plot
#' plot(catchments$geometry)
#' plot(gauges$geometry, add = TRUE, col = "red")
#' }
#'
#' @export
read_geospatial <- function(type = c("gauges", "catchments"),
                            path = get_camels_path()) {

  type <- match.arg(type)

  file_name <- switch(
    type,
    gauges = "camels_pe_gauges.gpkg",
    catchments = "camels_pe_catchments.gpkg"
  )

  file <- file.path(path, "04_geospatial", file_name)

  if (!file.exists(file)) {
    stop("Geospatial file not found: ", file_name)
  }

  data <- sf::st_read(file, quiet = TRUE)

  # Validate key column
  if (!"gauge_id" %in% names(data)) {
    warning(
      "Column 'gauge_id' not found in geospatial data. ",
      "Ensure consistency with CAMELS-PE structure."
    )
  }

  data
}
