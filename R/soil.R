#' Read CAMELS-PE Soil Classes
#'
#' Reads the areal percentages of the soil suborders, great groups and
#' horizon texture classes of the CAMELS-PE catchments, derived from the
#' Digital Soil Map DSOLMap (USDA Soil Taxonomy). The available levels are:
#'
#' \itemize{
#'   \item \code{"suborder"}: soil suborders (e.g. Ochrepts, Udults).
#'   \item \code{"great_group"}: soil great groups (e.g. cryochrepts,
#'   tropudults).
#'   \item \code{"texture_h1"}, \code{"texture_h2"}, \code{"texture_h3"}: USDA
#'   texture classes of the three horizons with which DSOLMap describes each
#'   soil unit, from the shallowest (\code{h1}) to the deepest (\code{h3}).
#' }
#'
#' The percentages of the soil orders and the dominant order, suborder, great
#' group and horizon textures are read with \code{read_attributes("soil")}.
#' Use \code{read_soil_legend()} to see the hierarchy order > suborder > great
#' group.
#'
#' @param level Character vector or \code{NULL}. One or more of
#'   \code{"suborder"}, \code{"great_group"}, \code{"texture_h1"},
#'   \code{"texture_h2"} and \code{"texture_h3"}. If \code{NULL}, all levels
#'   are returned. With \code{format = "wide"}, a single level must be given.
#' @param gauge_id Character vector or \code{NULL}. Optional gauge identifiers
#'   used to filter the data. If \code{NULL}, all catchments are returned.
#' @param format Character string. \code{"long"} (default) returns the table as
#'   stored in the dataset, one row per catchment, level and class, with the
#'   class name, order and suborder from the legend. \code{"wide"} returns one
#'   row per catchment and one \code{<class>_perc} column per class.
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, the path previously defined with \code{set_camels_path()}
#'   is used.
#'
#' @return A tibble with the soil class percentages of each catchment.
#'
#' @details
#' The data are read from \code{02_attributes/soil_classes.csv}. All classes
#' present in at least one catchment are listed, with 0 where absent, and the
#' classes of each level sum to 100\% in each catchment.
#'
#' The suborders and great groups follow the DSOLMap legend, which uses names
#' of the USDA Soil Taxonomy prior to 1999 (e.g. Ochrepts). DSOLMap does not
#' give the depth of the horizons.
#'
#' In long format, the columns are \code{gauge_id}, \code{level},
#' \code{class}, \code{name}, \code{order}, \code{suborder} and \code{perc}.
#'
#' @examples
#' path <- system.file(
#'   "extdata",
#'   "sample_camels_pe",
#'   package = "RCamelsPE"
#' )
#'
#' # Suborders of one catchment, in long format
#' sub <- read_soil_classes(
#'   level = "suborder",
#'   gauge_id = "PE_212900",
#'   path = path
#' )
#' sub[sub$perc > 0, ]
#'
#' # Texture of the shallowest horizon, one column per texture class
#' read_soil_classes(level = "texture_h1", format = "wide", path = path)
#' @export
read_soil_classes <- function(level = NULL,
                              gauge_id = NULL,
                              format = c("long", "wide"),
                              path = get_camels_path()) {

  format <- match.arg(format)
  levels <- c("suborder", "great_group", "texture_h1", "texture_h2",
              "texture_h3")

  if (!is.null(level)) {
    if (!is.character(level) || !all(level %in% levels)) {
      stop("`level` must be NULL or one or more of: ",
           paste(levels, collapse = ", "), call. = FALSE)
    }
  }

  if (format == "wide" && length(level) != 1L) {
    stop("`format = \"wide\"` requires a single `level`.", call. = FALSE)
  }

  file <- file.path(path, "02_attributes", "soil_classes.csv")

  if (!file.exists(file)) {
    stop(
      "Soil file not found: soil_classes.csv",
      call. = FALSE
    )
  }

  data <- readr::read_csv(
    file,
    col_types = readr::cols(
      gauge_id = readr::col_character(),
      level = readr::col_character(),
      class = readr::col_character(),
      perc = readr::col_double()
    ),
    show_col_types = FALSE,
    progress = FALSE,
    num_threads = 1
  )

  if (!is.null(level)) {
    data <- dplyr::filter(data, .data$level %in% .env$level)
  }

  if (!is.null(gauge_id)) {
    data <- dplyr::filter(data, .data$gauge_id %in% .env$gauge_id)
  }

  if (format == "wide") {
    classes <- unique(data$class)
    ids <- unique(data$gauge_id)
    out <- data.frame(gauge_id = ids, stringsAsFactors = FALSE)
    for (cl in classes) {
      sel <- data[data$class == cl, , drop = FALSE]
      out[[paste0(cl, "_perc")]] <- sel$perc[match(ids, sel$gauge_id)]
    }
    return(dplyr::as_tibble(out))
  }

  # Names, order and suborder from the legend; textures share one legend level
  legend <- read_soil_legend(path)
  data$legend_level <- ifelse(grepl("^texture", data$level), "texture",
                              data$level)
  out <- dplyr::left_join(
    data,
    legend,
    by = c("legend_level" = "level", "class" = "class")
  )
  out <- out[, c("gauge_id", "level", "class", "name", "order", "suborder",
                 "perc")]
  dplyr::as_tibble(out)
}


#' Read the CAMELS-PE Soil Legend
#'
#' Reads the soil legend of CAMELS-PE, which lists the soil suborders and
#' great groups present in the catchments, with the order and suborder they
#' belong to (USDA Soil Taxonomy, as in the DSOLMap legend), and the 12 USDA
#' texture classes.
#'
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, the path previously defined with \code{set_camels_path()}
#'   is used.
#'
#' @return A tibble with the columns \code{level} (\code{"suborder"},
#'   \code{"great_group"} or \code{"texture"}), \code{class} (code used in
#'   \code{soil_classes.csv} and in the soil attributes), \code{name},
#'   \code{order} and \code{suborder}.
#'
#' @examples
#' path <- system.file(
#'   "extdata",
#'   "sample_camels_pe",
#'   package = "RCamelsPE"
#' )
#'
#' read_soil_legend(path = path)
#' @export
read_soil_legend <- function(path = get_camels_path()) {

  file <- file.path(path, "02_attributes", "soil_classification.csv")

  if (!file.exists(file)) {
    stop(
      "Soil file not found: soil_classification.csv",
      call. = FALSE
    )
  }

  readr::read_csv(
    file,
    col_types = readr::cols(.default = readr::col_character()),
    na = "NA",
    show_col_types = FALSE,
    progress = FALSE,
    num_threads = 1
  )
}
