#' Download CAMELS-PE dataset from Zenodo
#'
#' Downloads and optionally extracts the CAMELS-PE dataset hosted on Zenodo.
#'
#' @param path Character. Directory where the dataset will be stored. This
#'   argument is required. In examples, use \code{tempdir()}.
#' @param version Character. Dataset version: \code{"2.0"} (default, latest),
#'   \code{"1.0.1"} or \code{"1.0"}. The reading functions of this package
#'   are designed for version 2.0; earlier versions are available only for
#'   reproducibility of previous studies and have a different file
#'   structure and variable names.
#' @param unzip Logical. If \code{TRUE}, the downloaded ZIP file is
#'   automatically extracted.
#' @param overwrite Logical. If \code{TRUE}, existing ZIP files will be
#'   overwritten.
#' @param set_path Logical. If \code{TRUE}, automatically sets the CAMELS-PE
#'   root directory using \code{set_camels_path()} after extraction.
#'
#' @return Invisibly returns the path to the downloaded ZIP file.
#'
#' @details
#' The CAMELS-PE dataset is distributed separately from the package through
#' Zenodo. The concept DOI \doi{10.5281/zenodo.20058778} refers to all
#' versions and resolves to the latest one. The DOIs of the individual
#' versions are:
#'
#' \itemize{
#'   \item v2.0: \doi{10.5281/zenodo.23890227}
#'   \item v1.0.1: \doi{10.5281/zenodo.21195425}
#'   \item v1.0: \doi{10.5281/zenodo.20058779}
#' }
#'
#' @examples
#' \donttest{
#' download_camels_pe(
#'   path = tempdir(),
#'   unzip = FALSE
#' )
#' }
#'
#' @export
download_camels_pe <- function(path,
                               version = "2.0",
                               unzip = TRUE,
                               overwrite = FALSE,
                               set_path = TRUE) {

  if (missing(path) || is.null(path) || length(path) != 1L || is.na(path)) {
    stop(
      "Please provide a valid destination directory through `path`.",
      call. = FALSE
    )
  }

  if (!is.character(path)) {
    stop("`path` must be a character string.", call. = FALSE)
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  # Zenodo record of each version
  records <- c(
    "2.0" = "23890227",
    "1.0.1" = "21195425",
    "1.0" = "20058779"
  )

  if (!is.character(version) || length(version) != 1L ||
      !version %in% names(records)) {
    stop(
      "Unsupported CAMELS-PE version. Available versions: ",
      paste(names(records), collapse = ", "),
      call. = FALSE
    )
  }

  url <- paste0(
    "https://zenodo.org/records/", records[[version]],
    "/files/CAMELS-PE_v", version, ".zip?download=1"
  )

  zip_file <- file.path(
    path,
    paste0("CAMELS-PE_v", version, ".zip")
  )

  if (file.exists(zip_file) && !overwrite) {
    stop(
      "File already exists:\n",
      zip_file,
      "\nUse overwrite = TRUE to overwrite the existing file.",
      call. = FALSE
    )
  }

  message("Downloading CAMELS-PE dataset...")

  old_timeout <- getOption("timeout")
  options(timeout = max(300, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  status <- tryCatch(
    {
      utils::download.file(
        url = url,
        destfile = zip_file,
        mode = "wb",
        quiet = FALSE
      )
      TRUE
    },
    error = function(e) {
      message(e$message)
      FALSE
    }
  )

  if (!status || !file.exists(zip_file)) {
    stop(
      "Failed to download CAMELS-PE dataset from Zenodo.\n",
      "The dataset may be temporarily unavailable.",
      call. = FALSE
    )
  }

  message("Download completed:")
  message(zip_file)

  if (unzip) {
    message("Extracting dataset...")

    utils::unzip(
      zipfile = zip_file,
      exdir = path
    )

    message("Dataset extracted to:")
    message(path)

    extracted_path <- file.path(path, "CAMELS-PE")

    if (!dir.exists(extracted_path)) {
      warning(
        "Dataset extracted, but the CAMELS-PE root folder ",
        "was not detected automatically.",
        call. = FALSE
      )
    }

    if (set_path && dir.exists(extracted_path)) {
      set_camels_path(extracted_path)

      message("CAMELS-PE path set to:")
      message(extracted_path)
    }
  }

  invisible(zip_file)
}
