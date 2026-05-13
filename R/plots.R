#' Plot CAMELS-PE Time Series
#'
#' Creates a time series plot for one CAMELS-PE variable, such as precipitation,
#' observed streamflow, simulated streamflow, or temperature. The function
#' returns a \code{ggplot} object, so it can be further customized using standard
#' \code{ggplot2} layers.
#'
#' Use \code{read_dictionary(category = "timeseries")} to inspect available
#' time series variables, descriptions, units, and data sources.
#'
#' @param data A data frame or tibble containing CAMELS-PE time series.
#' @param variable Character string. Name of the variable to plot.
#' @param gauge_id Optional character vector. Gauge IDs used to filter the data
#'   before plotting. If \code{NULL}, all available gauges are plotted.
#' @param date_col Character string. Name of the date column.
#' @param facet Logical value. If \code{TRUE}, creates one panel per gauge ID.
#' @param scales Character string. Scales passed to \code{ggplot2::facet_wrap()}.
#'   One of \code{"fixed"}, \code{"free"}, \code{"free_x"}, or \code{"free_y"}.
#' @param ... Additional arguments passed to \code{ggplot2::geom_line()}.
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
#' # Inspect available time series variables
#' read_dictionary(
#'   category = "timeseries",
#'   path = path
#' )
#'
#' ts <- read_timeseries(
#'   gauge_id = c("PE_212900", "PE_200907"),
#'   vars = c(
#'     "date",
#'     "gauge_id",
#'     "prec",
#'     "flow_obs",
#'     "flow_sim",
#'     "tmean"
#'   ),
#'   path = path
#' )
#'
#' plot_timeseries(ts, variable = "flow_obs")
#' plot_timeseries(ts, variable = "prec", linewidth = 0.4)
#' plot_timeseries(ts, variable = "flow_obs", facet = FALSE)
#' plot_timeseries(ts, variable = "flow_obs", gauge_id = "PE_212900")
#' @export
plot_timeseries <- function(data,
                            variable = "flow_obs",
                            gauge_id = NULL,
                            date_col = "date",
                            facet = TRUE,
                            scales = "free_y",
                            ...) {

  # ------------------------------------------------------------------
  # Input checks
  # ------------------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.character(variable) || length(variable) != 1L || is.na(variable)) {
    stop("`variable` must be a single character string.", call. = FALSE)
  }

  if (!is.character(date_col) || length(date_col) != 1L || is.na(date_col)) {
    stop("`date_col` must be a single character string.", call. = FALSE)
  }

  if (!date_col %in% names(data)) {
    stop("Date column not found in data: ", date_col, call. = FALSE)
  }

  if (!"gauge_id" %in% names(data)) {
    stop("Column 'gauge_id' not found in data.", call. = FALSE)
  }

  if (!variable %in% names(data)) {
    stop("Variable not found in data: ", variable, call. = FALSE)
  }

  if (!is.null(gauge_id) &&
      (!is.character(gauge_id) || anyNA(gauge_id))) {
    stop("`gauge_id` must be a character vector without NA values or NULL.",
         call. = FALSE)
  }

  if (!is.logical(facet) || length(facet) != 1L || is.na(facet)) {
    stop("`facet` must be TRUE or FALSE.", call. = FALSE)
  }

  allowed_scales <- c("fixed", "free", "free_x", "free_y")
  if (!is.character(scales) ||
      length(scales) != 1L ||
      is.na(scales) ||
      !scales %in% allowed_scales) {
    stop(
      "`scales` must be one of: ",
      paste(allowed_scales, collapse = ", "),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # Prepare data
  # ------------------------------------------------------------------
  data <- as.data.frame(data)

  if (!is.null(gauge_id)) {
    data <- data[data[["gauge_id"]] %in% gauge_id, , drop = FALSE]
  }

  if (nrow(data) == 0L) {
    stop("No data available for the selected `gauge_id`.", call. = FALSE)
  }

  data[[date_col]] <- as.Date(data[[date_col]])

  if (anyNA(data[[date_col]])) {
    stop("Date column contains values that cannot be converted to Date.",
         call. = FALSE)
  }

  if (!is.numeric(data[[variable]])) {
    stop("`variable` must refer to a numeric column.", call. = FALSE)
  }

  # Do not remove NA values to preserve gaps in time series

  if (nrow(data) == 0L) {
    stop("No non-missing values available for `variable`.", call. = FALSE)
  }

  data[["gauge_id"]] <- as.character(data[["gauge_id"]])
  data <- data[order(data[["gauge_id"]], data[[date_col]]), , drop = FALSE]

  # ------------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------------
  if (facet) {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data[[date_col]],
        y = .data[[variable]]
      )
    ) +
      ggplot2::geom_line(color = "#636EFA", ...) +
      ggplot2::facet_wrap(ggplot2::vars(.data$gauge_id), scales = scales)
  } else {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data[[date_col]],
        y = .data[[variable]],
        color = .data$gauge_id
      )
    ) +
      ggplot2::geom_line(...) +
      ggplot2::labs(color = "Gauge ID")
  }

  p +
    ggplot2::labs(
      x = NULL,
      y = variable
    ) +
    ggplot2::theme_bw()
}


#' Plot CAMELS-PE Catchments
#'
#' Creates a map of CAMELS-PE catchments and optionally overlays gauge
#' locations. If \code{gauge_id} is provided, only one catchment and its outlet
#' are plotted, with the gauge ID shown as a panel header.
#'
#' Use \code{read_dictionary(category = "geospatial")} to inspect available
#' geospatial layers and their descriptions.
#'
#' @param catchments An \code{sf} object with catchment polygons.
#' @param gauges Optional \code{sf} object with gauge point locations.
#' @param gauge_id Optional character string. Gauge ID used to filter one
#'   catchment and its gauge before plotting. If \code{NULL}, all catchments are
#'   plotted.
#' @param fill Optional character string. Name of a catchment column used to
#'   fill polygons. If \code{NULL}, a constant fill is used.
#' @param ... Additional arguments passed to \code{ggplot2::geom_sf()}.
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
#' # Inspect available geospatial layers
#' read_dictionary(
#'   category = "geospatial",
#'   path = path
#' )
#'
#' catchments <- read_geospatial(
#'   type = "catchments",
#'   path = path
#' )
#'
#' gauges <- read_geospatial(
#'   type = "gauges",
#'   path = path
#' )
#'
#' plot_catchments(catchments, gauges)
#'
#' plot_catchments(
#'   catchments,
#'   gauges,
#'   gauge_id = "PE_212900"
#' )
#' @export
plot_catchments <- function(catchments,
                            gauges = NULL,
                            gauge_id = NULL,
                            fill = NULL,
                            ...) {

  if (!inherits(catchments, "sf")) {
    stop("`catchments` must be an sf object.", call. = FALSE)
  }

  if (!is.null(gauges) && !inherits(gauges, "sf")) {
    stop("`gauges` must be an sf object.", call. = FALSE)
  }

  if (!"gauge_id" %in% names(catchments)) {
    stop("Column 'gauge_id' not found in catchments.", call. = FALSE)
  }

  if (!is.null(gauges) && !"gauge_id" %in% names(gauges)) {
    stop("Column 'gauge_id' not found in gauges.", call. = FALSE)
  }

  if (!is.null(gauge_id) &&
      (!is.character(gauge_id) || length(gauge_id) != 1 || is.na(gauge_id))) {
    stop("`gauge_id` must be a single character string or NULL.",
         call. = FALSE)
  }

  if (!is.null(fill)) {
    if (!is.character(fill) || length(fill) != 1 || is.na(fill)) {
      stop("`fill` must be a single character string or NULL.",
           call. = FALSE)
    }

    if (!fill %in% names(catchments)) {
      stop("Fill variable not found in catchments: ", fill,
           call. = FALSE)
    }
  }

  catchments$gauge_id <- as.character(catchments$gauge_id)

  if (!is.null(gauges)) {
    gauges$gauge_id <- as.character(gauges$gauge_id)
  }

  if (!is.null(gauge_id)) {
    catchments <- catchments[catchments[["gauge_id"]] == gauge_id, ]

    if (!is.null(gauges)) {
      gauges <- gauges[gauges[["gauge_id"]] == gauge_id, ]
    }
  }

  if (nrow(catchments) == 0) {
    stop("No catchment available for the selected gauge_id.",
         call. = FALSE)
  }

  if (is.null(fill)) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = catchments,
        fill = "grey90",
        color = "grey40",
        linewidth = 0.2,
        ...
      )
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = catchments,
        ggplot2::aes(fill = .data[[fill]]),
        color = "grey40",
        linewidth = 0.2,
        ...
      ) +
      ggplot2::scale_fill_viridis_c(na.value = "grey80") +
      ggplot2::labs(fill = fill)
  }

  if (!is.null(gauges) && nrow(gauges) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = gauges,
        size = 1.5,
        color = "black"
      )
  }

  if (!is.null(gauge_id)) {
    p <- p +
      ggplot2::facet_wrap(~gauge_id)
  }

  p +
    ggplot2::theme_bw() +
    ggplot2::labs(x = NULL, y = NULL)
}


#' Plot CAMELS-PE Attribute Map
#'
#' Creates a thematic map of CAMELS-PE catchments using a selected attribute.
#' The function joins attribute data with catchment geometries by
#' \code{gauge_id} and returns a \code{ggplot} object.
#'
#' Use \code{read_dictionary()} to inspect available attributes,
#' descriptions, units, categories, source files, and data sources.
#'
#' @param catchments An \code{sf} object with catchment polygons.
#' @param attributes A data frame or tibble with CAMELS-PE attributes.
#' @param variable Character string. Name of the attribute to visualize.
#' @param gauges Optional \code{sf} object with gauge point locations.
#' @param na_color Character string. Color for missing values.
#' @param ... Additional arguments passed to \code{ggplot2::geom_sf()} for
#'   catchments.
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
#' # Inspect available topographic attributes
#' read_dictionary(
#'   category = "topographic",
#'   path = path
#' )
#'
#' catchments <- read_geospatial(
#'   type = "catchments",
#'   path = path
#' )
#'
#' gauges <- read_geospatial(
#'   type = "gauges",
#'   path = path
#' )
#'
#' attr <- read_attributes(
#'   type = "topographic",
#'   path = path
#' )
#'
#' plot_attribute_map(
#'   catchments = catchments,
#'   attributes = attr,
#'   variable = "area",
#'   gauges = gauges
#' )
#' @export
plot_attribute_map <- function(catchments,
                               attributes,
                               variable,
                               gauges = NULL,
                               na_color = "grey80",
                               ...) {

  # ------------------------------------------------------------------
  # Checks
  # ------------------------------------------------------------------
  if (!inherits(catchments, "sf")) {
    stop("`catchments` must be an sf object.", call. = FALSE)
  }

  if (!is.data.frame(attributes)) {
    stop("`attributes` must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.character(variable) || length(variable) != 1) {
    stop("`variable` must be a single character string.", call. = FALSE)
  }

  if (!"gauge_id" %in% names(catchments)) {
    stop("`catchments` must contain a 'gauge_id' column.", call. = FALSE)
  }

  if (!"gauge_id" %in% names(attributes)) {
    stop("`attributes` must contain a 'gauge_id' column.", call. = FALSE)
  }

  if (!variable %in% names(attributes)) {
    stop("Variable not found in attributes: ", variable, call. = FALSE)
  }

  if (!is.null(gauges) && !inherits(gauges, "sf")) {
    stop("`gauges` must be an sf object.", call. = FALSE)
  }

  if (!is.character(na_color) || length(na_color) != 1) {
    stop("`na_color` must be a single character string.", call. = FALSE)
  }

  # ------------------------------------------------------------------
  # Keep only geometry + gauge_id to avoid duplicated columns
  # ------------------------------------------------------------------
  catchments_min <- catchments[, "gauge_id"]

  # ------------------------------------------------------------------
  # Join attributes
  # ------------------------------------------------------------------
  data <- dplyr::left_join(catchments_min, attributes, by = "gauge_id")

  if (!variable %in% names(data)) {
    stop("Variable not found after join: ", variable, call. = FALSE)
  }

  # ------------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = data,
      ggplot2::aes(fill = .data[[variable]]),
      color = "grey70",
      linewidth = 0.15,
      ...
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "viridis",
      na.value = na_color
    )

  # ------------------------------------------------------------------
  # Add gauges
  # ------------------------------------------------------------------
  if (!is.null(gauges) && nrow(gauges) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = gauges,
        size = 1,
        color = "#636EFA"
      )
  }

  # ------------------------------------------------------------------
  # Final theme
  # ------------------------------------------------------------------
  p +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = variable
    ) +
    ggplot2::theme_bw()
}
