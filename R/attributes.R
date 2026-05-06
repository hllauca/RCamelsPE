#' Read CAMELS-PE Catchment Attributes
#'
#' Reads catchment attributes from the CAMELS-PE dataset. Attributes are stored
#' in thematic groups, including topography, climate, hydrological signatures,
#' land cover, geology, soil, and human intervention.
#'
#' The function can read one attribute group at a time or join all available
#' groups by \code{gauge_id}. Use \code{read_dictionary()} to inspect available
#' variable names, descriptions, units, categories, source files, and data
#' sources.
#'
#' Available attribute groups are:
#'
#' \itemize{
#'   \item \code{"topographic"}: topographic and physiographic attributes.
#'   \item \code{"climatic"}: climatic indices and long-term climate descriptors.
#'   \item \code{"hydrological"}: hydrological signatures.
#'   \item \code{"landcover"}: land-cover attributes.
#'   \item \code{"geologic"}: geological attributes.
#'   \item \code{"soil"}: soil attributes.
#'   \item \code{"human_intervention"}: human intervention and regulation attributes.
#'   \item \code{"all"}: all available attribute groups joined by \code{gauge_id}.
#' }
#'
#' @param type Character string. Attribute group to read. One of
#'   \code{"topographic"}, \code{"climatic"}, \code{"hydrological"},
#'   \code{"landcover"}, \code{"geologic"}, \code{"soil"},
#'   \code{"human_intervention"}, or \code{"all"}. Default is \code{"all"}.
#' @param gauge_id Character vector or \code{NULL}. Optional gauge identifiers
#'   used to filter the returned attributes, for example \code{"PE_250101"} or
#'   \code{c("PE_250101", "PE_200907")}. If \code{NULL}, all available catchments
#'   are returned.
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, the path previously defined with \code{set_camels_path()}
#'   is used.
#'
#' @return A tibble with CAMELS-PE catchment attributes. The output includes
#'   one row per catchment and a \code{gauge_id} column used as the main
#'   identifier. When \code{type = "all"}, attribute groups are joined by
#'   \code{gauge_id}.
#'
#' @details
#' Attribute files are expected to be stored in \code{02_attributes/}. Each
#' attribute file must contain a \code{gauge_id} column. When
#' \code{type = "all"}, missing attribute files are skipped with a warning and
#' all available files are joined. If no valid attribute files are found, the
#' function stops with an error.
#'
#' Expected files are:
#'
#' \itemize{
#'   \item \code{topographic_attributes.csv}
#'   \item \code{climatic_indices.csv}
#'   \item \code{hydrological_signatures.csv}
#'   \item \code{landcover_attributes.csv}
#'   \item \code{geologic_attributes.csv}
#'   \item \code{soil_attributes.csv}
#'   \item \code{human_intervention_attributes.csv}
#' }
#'
#' @examples
#' \dontrun{
#' set_camels_path("D:/DATA/CAMELS-PE")
#'
#' # Inspect available topographic attributes
#' dict <- read_dictionary(category = "topographic")
#' head(dict)
#'
#' # Read topographic attributes
#' topo <- read_attributes(type = "topographic")
#'
#' # Read climatic attributes for selected catchments
#' clim <- read_attributes(
#'   type = "climatic",
#'   gauge_id = c("PE_250101", "PE_200907")
#' )
#'
#' # Read all available attribute groups
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
      paste(c(names(files), "all"), collapse = ", "),
      call. = FALSE
    )
  }

  attr_path <- file.path(path, "02_attributes")

  if (type == "all") {

    data_list <- lapply(names(files), function(attribute_type) {
      file <- file.path(attr_path, files[[attribute_type]])

      if (!file.exists(file)) {
        warning(
          "Attribute file not found: ",
          files[[attribute_type]],
          call. = FALSE
        )
        return(NULL)
      }

      data <- readr::read_csv(file, show_col_types = FALSE)

      if (!"gauge_id" %in% names(data)) {
        stop(
          "The attribute file '", files[[attribute_type]],
          "' must contain a 'gauge_id' column.",
          call. = FALSE
        )
      }

      data
    })

    data_list <- data_list[!vapply(data_list, is.null, logical(1))]

    if (length(data_list) == 0) {
      stop("No valid attribute files were found.", call. = FALSE)
    }

    data <- Reduce(function(x, y) {
      dplyr::full_join(x, y, by = "gauge_id")
    }, data_list)

  } else {

    file <- file.path(attr_path, files[[type]])

    if (!file.exists(file)) {
      stop("Attribute file not found: ", files[[type]], call. = FALSE)
    }

    data <- readr::read_csv(file, show_col_types = FALSE)

    if (!"gauge_id" %in% names(data)) {
      stop(
        "The attribute file '", files[[type]],
        "' must contain a 'gauge_id' column.",
        call. = FALSE
      )
    }
  }

  if (!is.null(gauge_id)) {
    data <- dplyr::filter(data, .data$gauge_id %in% gauge_id)
  }

  data
}
