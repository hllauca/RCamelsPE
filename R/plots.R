# =========================================================
# Time series plots
# =========================================================

#' Plot CAMELS-PE Time Series
#'
#' Creates a time series plot for one CAMELS-PE variable, such as precipitation,
#' observed streamflow, or temperature. The function returns a \code{ggplot}
#' object, so it can be further customized using standard \code{ggplot2}
#' layers.
#'
#' @param data A data frame or tibble containing CAMELS-PE time series.
#' @param variable Character string. Name of the variable to plot.
#' @param gauge_id Optional character vector. Gauge IDs to filter before plotting.
#' @param date_col Character string. Name of the date column.
#' @param facet Logical. If TRUE, creates one panel per gauge ID.
#' @param scales Character string. Scales passed to \code{facet_wrap()}.
#' @param ... Additional arguments passed to \code{ggplot2::geom_line()}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' ts <- read_timeseries(
#'   gauge_id = c("PE_0001", "PE_0002"),
#'   vars = c("date", "gauge_id", "prec", "flow_obs", "tmean")
#' )
#'
#' plot_timeseries(ts, variable = "flow_obs")
#' plot_timeseries(ts, variable = "prec", linewidth = 0.4)
#' plot_timeseries(ts, variable = "flow_obs", facet = FALSE)
#' }
#'
#' @export
plot_timeseries <- function(data,
                            variable = "flow_obs",
                            gauge_id = NULL,
                            date_col = "date",
                            facet = TRUE,
                            scales = "free_y",
                            ...) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.")
  }

  if (!date_col %in% names(data)) {
    stop("Date column not found in data: ", date_col)
  }

  if (!"gauge_id" %in% names(data)) {
    stop("Column 'gauge_id' not found in data.")
  }

  if (!variable %in% names(data)) {
    stop("Variable not found in data: ", variable)
  }

  if (!is.logical(facet) || length(facet) != 1) {
    stop("`facet` must be TRUE or FALSE.")
  }

  if (!is.null(gauge_id)) {
    data <- dplyr::filter(data, .data$gauge_id %in% gauge_id)
  }

  if (nrow(data) == 0) {
    stop("No data available for the selected gauge_id.")
  }

  data[[date_col]] <- as.Date(data[[date_col]])

  if (facet) {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data[[date_col]],
        y = .data[[variable]]
      )
    ) +
      ggplot2::geom_line(color = "#636EFA", ...)
      ggplot2::facet_wrap(~gauge_id, scales = scales)
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


# =========================================================
# Map plots
# =========================================================

#' Plot CAMELS-PE Catchments
#'
#' Creates a simple map of CAMELS-PE catchments and, optionally, gauge
#' locations. This function is useful for quick inspection of the spatial
#' distribution of catchments and stations.
#'
#' @param catchments An \code{sf} object with catchment polygons, typically
#'   obtained with \code{read_geospatial("catchments")}.
#' @param gauges Optional \code{sf} object with gauge point locations, typically
#'   obtained with \code{read_geospatial("gauges")}.
#' @param fill Optional character string. Name of a catchment column used to
#'   fill polygons. If NULL, catchments are plotted with a constant fill.
#' @param ... Additional arguments passed to \code{ggplot2::geom_sf()} for
#'   catchments.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' catchments <- read_geospatial("catchments")
#' gauges <- read_geospatial("gauges")
#'
#' plot_catchments(catchments)
#' plot_catchments(catchments, gauges)
#' }
#'
#' @export
plot_catchments <- function(catchments,
                            gauges = NULL,
                            fill = NULL,
                            ...) {

  if (!inherits(catchments, "sf")) {
    stop("`catchments` must be an sf object.")
  }

  if (!is.null(gauges) && !inherits(gauges, "sf")) {
    stop("`gauges` must be an sf object.")
  }

  if (!is.null(fill) && !fill %in% names(catchments)) {
    stop("Fill variable not found in catchments: ", fill)
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

  if (!is.null(gauges)) {
    p <- p +
      ggplot2::geom_sf(
        data = gauges,
        size = 1,
        color = "black"
      )
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
#' \dontrun{
#' catchments <- read_geospatial("catchments")
#' gauges <- read_geospatial("gauges")
#' attr <- read_attributes("topographic")
#'
#' plot_attribute_map(
#'   catchments = catchments,
#'   attributes = attr,
#'   variable = "area_km2",
#'   gauges = gauges
#' )
#' }
#'
#' @export
plot_attribute_map <- function(catchments,
                               attributes,
                               variable,
                               gauges = NULL,
                               na_color = "grey80",
                               ...) {

  if (!inherits(catchments, "sf")) {
    stop("`catchments` must be an sf object.")
  }

  if (!is.data.frame(attributes)) {
    stop("`attributes` must be a data frame or tibble.")
  }

  if (!"gauge_id" %in% names(catchments)) {
    stop("`catchments` must contain a 'gauge_id' column.")
  }

  if (!"gauge_id" %in% names(attributes)) {
    stop("`attributes` must contain a 'gauge_id' column.")
  }

  if (!variable %in% names(attributes)) {
    stop("Variable not found in attributes: ", variable)
  }

  if (!is.null(gauges) && !inherits(gauges, "sf")) {
    stop("`gauges` must be an sf object.")
  }

  data <- dplyr::left_join(catchments, attributes, by = "gauge_id")

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

  if (!is.null(gauges)) {
    p <- p +
      ggplot2::geom_sf(
        data = gauges,
        size = 1,
        color = "#636EFA"
      )
  }

  p +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = variable
    ) +
    ggplot2::theme_bw()
}
