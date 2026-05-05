#' Read CAMELS-PE time series
#'
#' Reads time series data from the CAMELS-PE dataset. The function supports
#' two modes:
#'
#' \itemize{
#'   \item \strong{Global mode}: reads the full dataset from \code{timeseries.csv}.
#'   \item \strong{By-catchment mode} (recommended): reads only selected
#'   catchments from individual files in \code{by_catchment/}.
#' }
#'
#' @param gauge_id Optional character vector of gauge IDs.
#'   Required if \code{global = FALSE}.
#' @param global Logical. If TRUE, reads the global file.
#'   Default is FALSE (recommended for performance).
#' @param vars Optional character vector of variables to keep.
#' @param path Optional CAMELS-PE root path.
#'
#' @return A tibble with time series data.
#'
#' @examples
#' \dontrun{
#' set_camels_path("D:/DATA/CAMELS-PE")
#'
#' # Read a single catchment
#' ts <- read_timeseries(gauge_id = "PE_0001")
#'
#' # Read multiple catchments
#' ts <- read_timeseries(gauge_id = c("PE_0001", "PE_0002"))
#'
#' # Read global dataset
#' ts_all <- read_timeseries(global = TRUE)
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
