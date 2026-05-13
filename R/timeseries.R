#' Read CAMELS-PE Time Series
#'
#' Reads hydrometeorological time series from the CAMELS-PE dataset. The
#' function supports two reading modes:
#'
#' \itemize{
#'   \item \strong{Global mode}: reads the full time series table from
#'   \code{03_timeseries/timeseries.csv}.
#'   \item \strong{By-catchment mode}: reads one or more catchment-specific
#'   files from \code{03_timeseries/by_catchment/}. This is the recommended
#'   mode for large datasets because only the requested catchments are loaded.
#' }
#'
#' The dataset must be available in a local directory previously defined with
#' \code{set_camels_path()}, or supplied directly through the \code{path}
#' argument.
#'
#' Available variables typically include:
#'
#' \itemize{
#'   \item \code{"date"}: date of observation.
#'   \item \code{"gauge_id"}: catchment identifier.
#'   \item \code{"prec"}: precipitation (mm/day).
#'   \item \code{"prec_var"}: precipitation variability or variance
#'   (mm2/day2).
#'   \item \code{"flow_obs"}: observed streamflow (mm/day).
#'   \item \code{"flow_sim"}: simulated streamflow (mm/day).
#'   \item \code{"tmean"}: mean air temperature (degrees C).
#'   \item \code{"tmax"}: maximum air temperature (degrees C).
#'   \item \code{"tmin"}: minimum air temperature (degrees C).
#'   \item \code{"pet"}: potential evapotranspiration (mm/day).
#'   \item \code{"srad"}: solar radiation (MJ/m2/day).
#'   \item \code{"vprp"}: vapor pressure (hPa).
#' }
#'
#' Available variables may differ depending on the CAMELS-PE dataset version.
#' Use \code{read_dictionary(category = "timeseries")} to inspect the complete
#' list of available variables, descriptions, units, and data sources.
#'
#' @param gauge_id Character vector or \code{NULL}. Gauge identifiers to read,
#'   for example \code{"PE_212900"} or \code{c("PE_212900", "PE_200907")}.
#'   This argument is required when \code{global = FALSE}. When
#'   \code{global = TRUE}, it can be used to filter the global time series file.
#' @param global Logical value. If \code{TRUE}, reads the global file
#'   \code{03_timeseries/timeseries.csv}. If \code{FALSE}, reads individual
#'   catchment files from \code{03_timeseries/by_catchment/}. Default is
#'   \code{FALSE}.
#' @param vars Character vector or \code{NULL}. Optional variable names to keep
#'   in the returned table. If \code{NULL}, all available variables are returned.
#'   Use \code{read_dictionary(category = "timeseries")} to inspect available
#'   variables.
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, the path set by \code{set_camels_path()} is used.
#'
#' @return A tibble containing CAMELS-PE time series data. The output usually
#'   includes at least \code{date}, \code{gauge_id}, and the selected
#'   hydrometeorological variables. If a \code{date} column is available, it is
#'   converted to class \code{Date}.
#'
#' @details
#' In by-catchment mode, each requested gauge is expected to have a file named
#' \code{<gauge_id>.csv} inside \code{03_timeseries/by_catchment/}. For example,
#' \code{PE_212900} is expected to be stored as
#' \code{03_timeseries/by_catchment/PE_212900.csv}.
#'
#' If one or more requested files are missing, the function issues a warning and
#' reads the files that are available. If no valid files are found, the function
#' stops with an error.
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
#' # Read all variables for one catchment
#' ts1 <- read_timeseries(
#'   gauge_id = "PE_212900",
#'   path = path
#' )
#'
#' # Read selected variables for multiple catchments
#' ts2 <- read_timeseries(
#'   gauge_id = c("PE_212900", "PE_200907"),
#'   vars = c("date", "gauge_id", "prec", "flow_obs"),
#'   path = path
#' )
#'
#' # Read the global time series file
#' ts_all <- read_timeseries(
#'   global = TRUE,
#'   path = path
#' )
#'
#' # Read the global file and filter selected gauges
#' ts_sel <- read_timeseries(
#'   gauge_id = c("PE_212900", "PE_200907"),
#'   global = TRUE,
#'   path = path
#' )
#' @export
read_timeseries <- function(gauge_id = NULL,
                            global = FALSE,
                            vars = NULL,
                            path = get_camels_path()) {

  if (!is.logical(global) || length(global) != 1) {
    stop("`global` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!global && is.null(gauge_id)) {
    stop("You must provide `gauge_id` when global = FALSE.", call. = FALSE)
  }

  if (global) {

    file <- file.path(path, "03_timeseries", "timeseries.csv")

    if (!file.exists(file)) {
      stop("Global file 'timeseries.csv' not found.", call. = FALSE)
    }

    data <- readr::read_csv(file, show_col_types = FALSE)

    if (!is.null(gauge_id)) {
      data <- dplyr::filter(data, .data$gauge_id %in% gauge_id)
    }

  } else {

    files <- file.path(
      path,
      "03_timeseries",
      "by_catchment",
      paste0(gauge_id, ".csv")
    )

    missing_files <- files[!file.exists(files)]

    if (length(missing_files) > 0) {
      warning(
        "Some files were not found:\n",
        paste(basename(missing_files), collapse = ", "),
        call. = FALSE
      )
    }

    files <- files[file.exists(files)]

    if (length(files) == 0) {
      stop("No valid catchment files found.", call. = FALSE)
    }

    data <- dplyr::bind_rows(lapply(files, function(f) {

      x <- readr::read_csv(f, show_col_types = FALSE)

      if (!"gauge_id" %in% names(x)) {
        x$gauge_id <- tools::file_path_sans_ext(basename(f))
      }

      x
    }))
  }

  if ("date" %in% names(data)) {
    data <- dplyr::mutate(data, date = as.Date(.data$date))
  }

  if (!is.null(vars)) {

    missing_vars <- setdiff(vars, names(data))

    if (length(missing_vars) > 0) {
      warning(
        "Some variables were not found:\n",
        paste(missing_vars, collapse = ", "),
        call. = FALSE
      )
    }

    vars <- intersect(vars, names(data))

    data <- dplyr::select(data, dplyr::all_of(vars))
  }

  data
}
