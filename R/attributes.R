#' Read CAMELS-PE Catchment Attributes
#'
#' Reads catchment attributes from the CAMELS-PE dataset. Attributes are stored
#' by thematic groups, such as topography, climate, hydrological signatures,
#' land cover, geology, soil, and human intervention.
#'
#' Available attribute types are:
#'
#' \itemize{
#'   \item \code{"topographic"}: topographic attributes.
#'   \item \code{"climatic"}: climatic indices.
#'   \item \code{"hydrological"}: hydrological signatures.
#'   \item \code{"landcover"}: land-cover attributes.
#'   \item \code{"geologic"}: geologic attributes.
#'   \item \code{"soil"}: soil attributes.
#'   \item \code{"human_intervention"}: human intervention attributes.
#'   \item \code{"all"}: all available attribute groups joined by \code{gauge_id}.
#' }
#'
#' @param type Character string. Attribute group to read. One of
#'   \code{"topographic"}, \code{"climatic"}, \code{"hydrological"},
#'   \code{"landcover"}, \code{"geologic"}, \code{"soil"},
#'   \code{"human_intervention"}, or \code{"all"}.
#' @param gauge_id Optional character vector. Gauge IDs used to filter the
#'   returned attributes.
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, the path previously defined with \code{set_camels_path()}
#'   is used.
#'
#' @return A tibble with CAMELS-PE catchment attributes.
#'
#' @examples
#' \dontrun{
#' set_camels_path("D:/DATA/CAMELS-PE")
#'
#' # Read topographic attributes
#' topo <- read_attributes(type = "topographic")
#'
#' # Read climatic attributes for selected catchments
#' clim <- read_attributes(
#'   type = "climatic",
#'   gauge_id = c("PE_0001", "PE_0002")
#' )
#'
#' # Read all attributes
#' attr_all <- read_attributes(type = "all")
#' }
#'
#' @export
read_attributes <- function(type = "all",
                            gauge_id = NULL,
                            path = get_camels_path()) {

  files <- c(
    topographic = "topographic_attributes.csv",
    climatic = "climatic_indices.csv",
    hydrological = "hydrological_signatures.csv",
    landcover = "landcover_attributes.csv",
    geologic = "geologic_attributes.csv",
    soil = "soil_attributes.csv",
    human_intervention = "human_intervention_attributes.csv"
  )

  if (!type %in% c(names(files), "all")) {
    stop(
      "Invalid attribute type. Use one of: ",
      paste(c(names(files), "all"), collapse = ", ")
    )
  }

  attr_path <- file.path(path, "02_attributes")

  if (type == "all") {

    data_list <- lapply(names(files), function(attribute_type) {
      file <- file.path(attr_path, files[[attribute_type]])

      if (!file.exists(file)) {
        warning("Attribute file not found: ", files[[attribute_type]])
        return(NULL)
      }

      data <- readr::read_csv(file, show_col_types = FALSE)

      if (!"gauge_id" %in% names(data)) {
        stop(
          "The attribute file '", files[[attribute_type]],
          "' must contain a 'gauge_id' column."
        )
      }

      data
    })

    data_list <- data_list[!vapply(data_list, is.null, logical(1))]

    if (length(data_list) == 0) {
      stop("No valid attribute files were found.")
    }

    data <- Reduce(function(x, y) {
      dplyr::full_join(x, y, by = "gauge_id")
    }, data_list)

  } else {

    file <- file.path(attr_path, files[[type]])

    if (!file.exists(file)) {
      stop("Attribute file not found: ", files[[type]])
    }

    data <- readr::read_csv(file, show_col_types = FALSE)

    if (!"gauge_id" %in% names(data)) {
      stop(
        "The attribute file '", files[[type]],
        "' must contain a 'gauge_id' column."
      )
    }
  }

  if (!is.null(gauge_id)) {
    data <- dplyr::filter(data, .data$gauge_id %in% gauge_id)
  }

  data
}
