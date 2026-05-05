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
#'   \item \code{"prec_var"}: precipitation variability or variance (mm²/day²).
#'   \item \code{"flow_obs"}: observed streamflow (mm/day).
#'   \item \code{"flow_sim"}: simulated streamflow (mm/day).
#'   \item \code{"tmean"}: mean air temperature (°C).
#'   \item \code{"tmax"}: maximum air temperature (°C).
#'   \item \code{"tmin"}: minimum air temperature (°C).
#'   \item \code{"pet"}: potential evapotranspiration (mm/day).
#'   \item \code{"srad"}: solar radiation (MJ/m²day).
#'   \item \code{"vprp"}: vapor pressure (hPa).
#' }
#'
#' Available variables may differ depending on the CAMELS-PE dataset version.
#'
#' @param gauge_id Character vector or \code{NULL}. Gauge identifiers to read,
#'   for example \code{"PE_0001"} or \code{c("PE_0001", "PE_0002")}.
#'   This argument is required when \code{global = FALSE}. When
#'   \code{global = TRUE}, it can be used to filter the global time series file.
#' @param global Logical. If \code{TRUE}, reads the global file
#'   \code{03_timeseries/timeseries.csv}. If \code{FALSE}, reads individual
#'   catchment files from \code{03_timeseries/by_catchment/}. Default is
#'   \code{FALSE}.
#' @param vars Character vector or \code{NULL}. Optional variable names to keep
#'   in the returned table. If \code{NULL}, all available variables are returned.
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
#' \code{PE_0001} is expected to be stored as
#' \code{03_timeseries/by_catchment/PE_0001.csv}.
#'
#' If one or more requested files are missing, the function issues a warning and
#' reads the files that are available. If no valid files are found, the function
#' stops with an error.
#'
#' @examples
#' \dontrun{
#' set_camels_path("D:/DATA/CAMELS-PE")
#'
#' # Read all variables for one catchment
#' ts1 <- read_timeseries(gauge_id = "PE_221804")
#'
#' # Read selected variables for multiple catchments
#' ts2 <- read_timeseries(
#'   gauge_id = c("PE_250101", "PE_200907"),
#'   vars = c("date", "gauge_id", "prec", "flow_obs")
#' )
#'
#' # Read the global time series file
#' ts_all <- read_timeseries(global = TRUE)
#'
#' # Read the global file and filter selected gauges
#' ts_sel <- read_timeseries(
#'   gauge_id = c("PE_250101", "PE_200907"),
#'   global = TRUE
#' )
#' }
#'
#' @export
read_timeseries <- function(gauge_id = NULL,
                            global = FALSE,
                            vars = NULL,
                            path = get_camels_path()) {

  # ---- 1. Validate inputs ----
  if (!is.logical(global) || length(global) != 1) {
    stop("`global` must be TRUE or FALSE.")
  }

  if (!global && is.null(gauge_id)) {
    stop("You must provide `gauge_id` when global = FALSE.")
  }

  # ---- 2. Global mode ----
  if (global) {

    file <- file.path(path, "03_timeseries", "timeseries.csv")

    if (!file.exists(file)) {
      stop("Global file 'timeseries.csv' not found.")
    }

    data <- readr::read_csv(file, show_col_types = FALSE)

    if (!is.null(gauge_id)) {
      data <- dplyr::filter(data, .data$gauge_id %in% gauge_id)
    }

  } else {

    # ---- 3. By-catchment mode (recommended) ----
    files <- file.path(
      path,
      "03_timeseries",
      "by_catchment",
      paste0(gauge_id, ".csv")
    )

    # Check missing files
    missing_files <- files[!file.exists(files)]

    if (length(missing_files) > 0) {
      warning(
        "Some files were not found:\n",
        paste(basename(missing_files), collapse = ", ")
      )
    }

    files <- files[file.exists(files)]

    if (length(files) == 0) {
      stop("No valid catchment files found.")
    }

    # Efficient row binding
    data <- dplyr::bind_rows(lapply(files, function(f) {

      x <- readr::read_csv(f, show_col_types = FALSE)

      # Ensure gauge_id exists
      if (!"gauge_id" %in% names(x)) {
        x$gauge_id <- tools::file_path_sans_ext(basename(f))
      }

      x
    }))
  }

  # ---- 4. Date handling ----
  if ("date" %in% names(data)) {
    data <- dplyr::mutate(data, date = as.Date(.data$date))
  }

  # ---- 5. Variable selection ----
  if (!is.null(vars)) {

    missing_vars <- setdiff(vars, names(data))

    if (length(missing_vars) > 0) {
      warning(
        "Some variables were not found:\n",
        paste(missing_vars, collapse = ", ")
      )
    }

    vars <- intersect(vars, names(data))

    data <- dplyr::select(data, dplyr::all_of(vars))
  }

  data
}
