#' Read CAMELS-PE Annual Land-Cover Series
#'
#' Reads the annual land-cover series (1985-2024) of the CAMELS-PE catchments,
#' derived from MapBiomas Peru, Ecuador and Colombia. Land cover is available
#' at two levels:
#'
#' \itemize{
#'   \item \code{"class"}: six aggregated classes (forest, agricultural area,
#'   natural non-forest formation, non-vegetated area, water and not observed),
#'   the same classes used in \code{land_cover_attributes.csv}.
#'   \item \code{"subclass"}: the MapBiomas subclasses (e.g. dry forest,
#'   pasture, scrubland, mining, glacier).
#' }
#'
#' Use \code{read_land_cover_legend()} to see how subclasses are grouped into
#' classes, and \code{read_dictionary(category = "land_cover")} to inspect
#' variable descriptions.
#'
#' @param level Character string. Either \code{"class"} or \code{"subclass"}.
#'   Default is \code{"class"}.
#' @param gauge_id Character vector or \code{NULL}. Optional gauge identifiers
#'   used to filter the series. If \code{NULL}, all catchments are returned.
#' @param years Numeric vector or \code{NULL}. Optional years to keep, for
#'   example \code{2000:2024}. If \code{NULL}, all years are returned.
#' @param format Character string. \code{"wide"} returns the table as stored
#'   in the dataset (one row per catchment and year, one \code{*_perc} column
#'   per class or subclass). \code{"long"} returns one row per catchment, year
#'   and class or subclass, with the names and codes of the legend and a
#'   \code{perc} column.
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, the path previously defined with \code{set_camels_path()}
#'   is used.
#'
#' @return A tibble with the annual land-cover percentages of each catchment.
#'
#' @details
#' The series are read from \code{02_attributes/land_cover_class_annual.csv}
#' and \code{02_attributes/land_cover_subclass_annual.csv}. Percentages refer
#' to the catchment area. In the class table, \code{water_perc} includes
#' glaciers.
#'
#' In long format, the class level returns the columns \code{gauge_id},
#' \code{year}, \code{class_code}, \code{class} and \code{perc}; the subclass
#' level also returns \code{subclass_code}, \code{subclass} and
#' \code{mapbiomas_id}.
#'
#' @examples
#' path <- system.file(
#'   "extdata",
#'   "sample_camels_pe",
#'   package = "RCamelsPE"
#' )
#'
#' # Annual classes, as stored in the dataset
#' lc_class <- read_land_cover(
#'   level = "class",
#'   path = path
#' )
#'
#' # Annual subclasses for one catchment since 2000, in long format
#' lc_sub <- read_land_cover(
#'   level = "subclass",
#'   gauge_id = "PE_212900",
#'   years = 2000:2024,
#'   format = "long",
#'   path = path
#' )
#'
#' head(lc_sub)
#' @export
read_land_cover <- function(level = c("class", "subclass"),
                            gauge_id = NULL,
                            years = NULL,
                            format = c("wide", "long"),
                            path = get_camels_path()) {

  level <- match.arg(level)
  format <- match.arg(format)

  if (!is.null(years) && !is.numeric(years)) {
    stop("`years` must be a numeric vector or NULL.", call. = FALSE)
  }

  file_name <- switch(
    level,
    class = "land_cover_class_annual.csv",
    subclass = "land_cover_subclass_annual.csv"
  )

  file <- file.path(path, "02_attributes", file_name)

  if (!file.exists(file)) {
    stop("Land-cover file not found: ", file_name, call. = FALSE)
  }

  types <- list(
    gauge_id = readr::col_character(),
    year = readr::col_integer(),
    .default = readr::col_double()
  )
  # Only the class table has the dominant class column
  if (level == "class") {
    types$land_dominant_class <- readr::col_character()
  }

  data <- readr::read_csv(
    file,
    col_types = do.call(readr::cols, types),
    show_col_types = FALSE,
    progress = FALSE,
    num_threads = 1
  )

  if (!is.null(gauge_id)) {
    data <- dplyr::filter(data, .data$gauge_id %in% .env$gauge_id)
  }

  if (!is.null(years)) {
    data <- dplyr::filter(data, .data$year %in% .env$years)
  }

  if (format == "long") {
    data <- land_cover_long(data, level, read_land_cover_legend(path))
  }

  data
}


#' Read the CAMELS-PE Land-Cover Legend
#'
#' Reads the land-cover legend of CAMELS-PE, which links each MapBiomas code
#' to its subclass and to the aggregated class used in the land-cover
#' attributes.
#'
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, the path previously defined with \code{set_camels_path()}
#'   is used.
#'
#' @return A tibble with the columns \code{mapbiomas_id}, \code{class},
#'   \code{class_code}, \code{subclass} and \code{subclass_code}. Column names
#'   in the annual series are \code{<class_code>_perc} and
#'   \code{<subclass_code>_perc}.
#'
#' @examples
#' path <- system.file(
#'   "extdata",
#'   "sample_camels_pe",
#'   package = "RCamelsPE"
#' )
#'
#' read_land_cover_legend(path = path)
#' @export
read_land_cover_legend <- function(path = get_camels_path()) {

  file <- file.path(path, "02_attributes", "land_cover_classification.csv")

  if (!file.exists(file)) {
    stop(
      "Land-cover file not found: land_cover_classification.csv",
      call. = FALSE
    )
  }

  readr::read_csv(
    file,
    col_types = readr::cols(
      mapbiomas_id = readr::col_integer(),
      .default = readr::col_character()
    ),
    show_col_types = FALSE,
    progress = FALSE,
    num_threads = 1
  )
}


#' Plot CAMELS-PE Annual Land Cover
#'
#' Creates a stacked area plot of the annual land-cover series of one or more
#' catchments. The function accepts the output of \code{read_land_cover()} in
#' wide or long format, at the class or subclass level, and returns a
#' \code{ggplot} object.
#'
#' @param data A data frame or tibble returned by \code{read_land_cover()}.
#' @param gauge_id Optional character vector. Gauge IDs used to filter the data
#'   before plotting. If \code{NULL}, all gauges in \code{data} are plotted, one
#'   panel per gauge.
#' @param min_perc Numeric value. Classes or subclasses whose maximum
#'   percentage over the period is below this value are grouped as
#'   \code{"Other"}. Default is \code{0}, which keeps all of them.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' path <- system.file(
#'   "extdata",
#'   "sample_camels_pe",
#'   package = "RCamelsPE"
#' )
#'
#' lc_class <- read_land_cover(level = "class", path = path)
#' plot_land_cover(lc_class)
#'
#' lc_sub <- read_land_cover(
#'   level = "subclass",
#'   gauge_id = "PE_212900",
#'   format = "long",
#'   path = path
#' )
#' plot_land_cover(lc_sub, min_perc = 1)
#' @export
plot_land_cover <- function(data,
                            gauge_id = NULL,
                            min_perc = 0) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  if (!all(c("gauge_id", "year") %in% names(data))) {
    stop("`data` must contain 'gauge_id' and 'year' columns.", call. = FALSE)
  }

  if (!is.numeric(min_perc) || length(min_perc) != 1L || is.na(min_perc)) {
    stop("`min_perc` must be a single number.", call. = FALSE)
  }

  # Convert wide tables to long; names are not needed for plotting
  if (!"perc" %in% names(data)) {
    level <- if ("forest_perc" %in% names(data)) "class" else "subclass"
    data <- land_cover_long(data, level, legend = NULL)
  }

  level <- if ("subclass_code" %in% names(data)) "subclass" else "class"
  code_col <- paste0(level, "_code")
  name_col <- if (level %in% names(data)) level else code_col

  if (!is.null(gauge_id)) {
    data <- data[data[["gauge_id"]] %in% gauge_id, , drop = FALSE]
  }

  if (nrow(data) == 0L) {
    stop("No data available for the selected `gauge_id`.", call. = FALSE)
  }

  data <- as.data.frame(data)
  data$cover <- as.character(data[[name_col]])

  # Group minor classes as "Other"
  max_perc <- tapply(data$perc, data$cover, max, na.rm = TRUE)
  minor <- names(max_perc)[max_perc < min_perc]
  data$cover[data$cover %in% minor] <- "Other"
  cover_levels <- unique(c(setdiff(unique(data$cover), "Other"), "Other"))
  data <- stats::aggregate(
    perc ~ gauge_id + year + cover,
    data = data,
    FUN = sum
  )
  # Keep the legend order of the dataset (subclasses grouped by class)
  data$cover <- factor(data$cover, levels = cover_levels)
  data <- droplevels(data)

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$year, y = .data$perc, fill = .data$cover)
  ) +
    ggplot2::geom_area(colour = "grey30", linewidth = 0.1) +
    ggplot2::facet_wrap(ggplot2::vars(.data$gauge_id)) +
    ggplot2::labs(x = NULL, y = "Catchment area (%)", fill = NULL) +
    ggplot2::theme_bw()

  if (level == "class") {
    # Colours close to the MapBiomas legend
    pal <- c(
      forest = "#1f8d49", "Forest formation" = "#1f8d49",
      agricul = "#e974ed", "Agricultural area" = "#e974ed",
      non_forest = "#d6bc74", "Natural non-forest formation" = "#d6bc74",
      non_veget = "#d4271e", "Non-vegetated area" = "#d4271e",
      water = "#2532e4", "Water body" = "#2532e4",
      non_identi = "#bdbdbd", "Not observed" = "#bdbdbd",
      Other = "grey60"
    )
    p <- p + ggplot2::scale_fill_manual(values = pal)
  }

  p
}


# Convert a wide annual land-cover table to long format. If `legend` is given,
# class and subclass names are added.
land_cover_long <- function(data, level, legend = NULL) {

  perc_cols <- grep("_perc$", names(data), value = TRUE)
  code_col <- paste0(level, "_code")

  out <- data.frame(
    gauge_id = rep(data$gauge_id, times = length(perc_cols)),
    year = rep(data$year, times = length(perc_cols)),
    code = rep(sub("_perc$", "", perc_cols), each = nrow(data)),
    perc = unlist(data[perc_cols], use.names = FALSE),
    stringsAsFactors = FALSE
  )
  names(out)[names(out) == "code"] <- code_col

  if (!is.null(legend)) {
    if (level == "class") {
      lookup <- unique(legend[, c("class_code", "class")])
    } else {
      lookup <- legend[, c("subclass_code", "subclass", "class_code",
                           "class", "mapbiomas_id")]
    }
    out <- dplyr::left_join(out, lookup, by = code_col)
    out <- dplyr::relocate(out, "perc", .after = dplyr::last_col())
  }

  out <- out[order(out$gauge_id, out$year), , drop = FALSE]
  dplyr::as_tibble(out)
}
