#' Download CAMELS-PE dataset from Zenodo
#'
#' Downloads and optionally extracts the CAMELS-PE dataset hosted on Zenodo.
#'
#' @param path Character. Directory where the dataset will be stored. This
#'   argument is required. In examples, use \code{tempdir()}.
#' @param version Character. Dataset version. Default is \code{"1.0"}.
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
#' Zenodo:
#'
#' \doi{10.5281/zenodo.20058779}
#'
#' @examples
#' \dontrun{
#' download_camels_pe(
#'   path = tempdir(),
#'   unzip = FALSE
#' )
#' }
#'
#' @export
download_camels_pe <- function(path,
                               version = "1.0",
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

  urls <- c(
    "1.0" = paste0(
      "https://zenodo.org/records/20058779/files/",
      "CAMELS-PE_v1.0.zip?download=1"
    )
  )

  if (!version %in% names(urls)) {
    stop(
      "Unsupported CAMELS-PE version. Available versions: ",
      paste(names(urls), collapse = ", "),
      call. = FALSE
    )
  }

  url <- urls[[version]]

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
