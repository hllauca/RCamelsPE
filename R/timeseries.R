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
#' Available variables include:
#'
#' \itemize{
#'   \item \code{"date"}: date of observation.
#'   \item \code{"gauge_id"}: catchment identifier (global file only; added
#'   from the file name in by-catchment mode).
#'   \item \code{"prec_pisco"}, \code{"prec_chirps"}, \code{"prec_era5land"}:
#'   precipitation from PISCOp, CHIRPS and ERA5-Land (mm/day).
#'   \item \code{"prec_sd_pisco"}: spatial standard deviation of PISCOp
#'   precipitation within the catchment (mm/day).
#'   \item \code{"pet_hs"}, \code{"pet_pt"}, \code{"pet_ou"}: potential
#'   evapotranspiration from Hargreaves-Samani, Priestley-Taylor and Oudin
#'   (mm/day).
#'   \item \code{"tmin"}, \code{"tmean"}, \code{"tmax"}: air temperature
#'   (degrees C).
#'   \item \code{"srad"}: solar radiation (MJ/m2/day).
#'   \item \code{"vprp"}: vapor pressure (hPa).
#'   \item \code{"flow_obs"}, \code{"flow_obs_m3s"}: observed streamflow after
#'   quality control (mm/day and m3/s).
#'   \item \code{"flow_obs_raw"}: observed streamflow before quality control
#'   (mm/day).
#'   \item \code{"flow_obs_flag"}: quality-control flag (1 = pass,
#'   3 = suspect, 4 = fail, 9 = missing).
#'   \item \code{"flow_obs_qc_test"}: quality-control test that raised the
#'   flag.
#'   \item \code{"flow_sim"}, \code{"flow_sim_m3s"}: simulated streamflow
#'   (mm/day and m3/s).
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
#'   Only the requested columns are read from disk, which speeds up reading
#'   the global file. Use \code{read_dictionary(category = "timeseries")} to
#'   inspect available variables.
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
#'   vars = c("date", "gauge_id", "prec_pisco", "flow_obs", "flow_obs_flag"),
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

    data <- read_ts_file(file, vars)

    if (!is.null(gauge_id)) {
      data <- dplyr::filter(data, .data$gauge_id %in% .env$gauge_id)
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

      x <- read_ts_file(f, vars, num_threads = 1)

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


# Read one time series file with fixed column types. Columns that are almost
# always NA (e.g. flow_obs_qc_test) would otherwise be guessed as logical.
# Several threads only pay off for the large global file.
read_ts_file <- function(file, vars = NULL,
                         num_threads = readr::readr_threads()) {

  header <- names(readr::read_csv(
    file,
    n_max = 0,
    show_col_types = FALSE,
    num_threads = 1
  ))

  types <- list(
    date = readr::col_date(),
    gauge_id = readr::col_character(),
    flow_obs_flag = readr::col_integer(),
    flow_obs_qc_test = readr::col_character()
  )
  types <- types[names(types) %in% header]

  col_types <- do.call(
    readr::cols,
    c(types, list(.default = readr::col_double()))
  )

  # Keep date and gauge_id for filtering; the final selection is done later
  keep <- header
  if (!is.null(vars)) {
    keep <- intersect(header, unique(c("date", "gauge_id", vars)))
  }

  readr::read_csv(
    file,
    col_types = col_types,
    col_select = dplyr::all_of(keep),
    show_col_types = FALSE,
    progress = FALSE,
    num_threads = num_threads
  )
}
