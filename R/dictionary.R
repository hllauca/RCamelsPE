#' Read CAMELS-PE data dictionary
#'
#' Reads the unified CAMELS-PE data dictionary containing metadata,
#' catchment attributes, geospatial layers, and time series variables.
#'
#' The dictionary provides information about variable names,
#' descriptions, units, categories, source files, and data sources
#' available in the CAMELS-PE dataset.
#'
#' Use this function to explore available variables before calling
#' functions such as \code{read_attributes()} or
#' \code{read_timeseries()}.
#'
#' @param category Character vector or \code{NULL}. Optional category
#'   filter. Examples include \code{"metadata"},
#'   \code{"topographic"}, \code{"climatic"},
#'   \code{"hydrological"}, \code{"landcover"},
#'   \code{"geologic"}, \code{"soil"},
#'   \code{"human_intervention"}, \code{"timeseries"},
#'   and \code{"geospatial"}.
#' @param variable Character vector or \code{NULL}. Optional variable
#'   names used to filter the dictionary.
#' @param file Character vector or \code{NULL}. Optional source file
#'   names used to filter the dictionary.
#' @param path Character string. Optional path to the CAMELS-PE root
#'   directory. If not provided, the path previously defined with
#'   \code{set_camels_path()} is used.
#'
#' @return A tibble containing the CAMELS-PE data dictionary.
#'
#' @details
#' The dictionary file is expected at:
#'
#' \code{01_metadata/data_dictionary.csv}
#'
#' The returned table includes:
#'
#' \itemize{
#'   \item \code{folder}: dataset folder location.
#'   \item \code{file}: source file name.
#'   \item \code{category}: thematic category.
#'   \item \code{variable}: variable name.
#'   \item \code{description}: variable description.
#'   \item \code{unit}: variable units.
#'   \item \code{source}: original data source.
#' }
#'
#' @examples
#' path <- system.file(
#'   "extdata",
#'   "sample_camels_pe",
#'   package = "RCamelsPE"
#' )
#'
#' # Read complete dictionary
#' dict <- read_dictionary(path = path)
#'
#' # Inspect topographic variables
#' topo_dict <- read_dictionary(
#'   category = "topographic",
#'   path = path
#' )
#'
#' # Search for a specific variable
#' flow_dict <- read_dictionary(
#'   variable = "flow_obs",
#'   path = path
#' )
#'
#' # Inspect variables from a specific file
#' stations_dict <- read_dictionary(
#'   file = "stations.csv",
#'   path = path
#' )
#' @export
read_dictionary <- function(category = NULL,
                            variable = NULL,
                            file = NULL,
                            path = get_camels_path()) {

  dictionary_file <- file.path(
    path,
    "01_metadata",
    "data_dictionary.csv"
  )

  if (!file.exists(dictionary_file)) {
    stop(
      "Data dictionary not found: ",
      dictionary_file,
      call. = FALSE
    )
  }

  dict <- readr::read_csv(
    dictionary_file,
    show_col_types = FALSE
  )

  required_cols <- c(
    "folder",
    "file",
    "category",
    "variable",
    "description",
    "unit",
    "source"
  )

  missing_cols <- setdiff(required_cols, names(dict))

  if (length(missing_cols) > 0) {
    stop(
      "The dictionary file is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(category)) {
    category_filter <- category

    dict <- dplyr::filter(
      dict,
      .data$category %in% category_filter
    )
  }

  if (!is.null(variable)) {
    variable_filter <- variable

    dict <- dplyr::filter(
      dict,
      .data$variable %in% variable_filter
    )
  }

  if (!is.null(file)) {
    file_filter <- file

    dict <- dplyr::filter(
      dict,
      .data$file %in% file_filter
    )
  }

  dict
}
