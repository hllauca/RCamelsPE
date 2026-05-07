#' Download CAMELS-PE dataset from Zenodo
#'
#' Downloads and optionally extracts the CAMELS-PE dataset
#' hosted on Zenodo.
#'
#' @param path Character. Directory where the dataset will be stored.
#'   Default is the current working directory.
#' @param version Character. Dataset version.
#'   Default is `"1.0"`.
#' @param unzip Logical. If `TRUE`, the downloaded ZIP file is
#'   automatically extracted.
#' @param overwrite Logical. If `TRUE`, existing ZIP files will be
#'   overwritten.
#' @param set_path Logical. If `TRUE`, automatically sets the
#'   CAMELS-PE root directory using `set_camels_path()` after extraction.
#'
#' @return Invisibly returns the path to the downloaded ZIP file.
#'
#' @details
#' The CAMELS-PE dataset is distributed separately from the package
#' through Zenodo:
#'
#' \doi{10.5281/zenodo.20058778}
#'
#' @examples
#' \dontrun{
#' download_camels_pe(path = "D:/CAMELS")
#' }
#'
#' @export

download_camels_pe <- function(path = getwd(),
                               version = "1.0",
                               unzip = TRUE,
                               overwrite = FALSE,
                               set_path = TRUE) {

  # Create destination directory
  if (!dir.exists(path)) {

    dir.create(
      path,
      recursive = TRUE
    )

  }

  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )

  # Zenodo download URLs
  urls <- c(
    "1.0" = paste0(
      "https://zenodo.org/records/20058779/files/",
      "CAMELS-PE_v1.0.zip?download=1"
    )
  )

  # Validate version
  if (!version %in% names(urls)) {

    stop(
      "Unsupported CAMELS-PE version. Available versions: ",
      paste(names(urls), collapse = ", "),
      call. = FALSE
    )

  }

  # Download URL
  url <- urls[[version]]

  # Output ZIP file
  zip_file <- file.path(
    path,
    paste0("CAMELS-PE_v", version, ".zip")
  )

  # Prevent overwrite
  if (file.exists(zip_file) && !overwrite) {

    stop(
      "File already exists:\n",
      zip_file,
      "\nUse overwrite = TRUE to overwrite the existing file.",
      call. = FALSE
    )

  }

  message("Downloading CAMELS-PE dataset...")

  # Increase timeout for large downloads
  old_timeout <- getOption("timeout")

  options(
    timeout = max(300, old_timeout)
  )

  on.exit(
    options(timeout = old_timeout),
    add = TRUE
  )

  # Download dataset
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

  # Validate download
  if (!status || !file.exists(zip_file)) {

    stop(
      "Failed to download CAMELS-PE dataset from Zenodo.\n",
      "The dataset may be temporarily unavailable.",
      call. = FALSE
    )

  }

  message("Download completed:")
  message(zip_file)

  # Extract dataset
  if (unzip) {

    message("Extracting dataset...")

    utils::unzip(
      zipfile = zip_file,
      exdir = path
    )

    message("Dataset extracted to:")
    message(path)

    # Detect CAMELS-PE root folder
    extracted_path <- file.path(
      path,
      "CAMELS-PE"
    )

    if (!dir.exists(extracted_path)) {

      warning(
        "Dataset extracted, but the CAMELS-PE root folder ",
        "was not detected automatically.",
        call. = FALSE
      )

    }

    # Set CAMELS-PE path
    if (set_path && dir.exists(extracted_path)) {

      set_camels_path(extracted_path)

      message("CAMELS-PE path set to:")
      message(extracted_path)

    }

  }

  invisible(zip_file)

}
