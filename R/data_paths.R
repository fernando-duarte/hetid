#' Path Management Utilities
#'
#' Path resolution functions for package data
#' that work consistently across development and installed package environments.
#'
#' @name data_paths
#' @keywords internal
NULL

#' Get Package Data Directory
#'
#' Returns the appropriate data directory path for both development and
#' installed package environments, ensuring reliable access to datasets.
#'
#' @return Character string with path to package data directory
#' @keywords internal
get_package_data_dir <- function() {
  # Path resolution for package data
  pkg_dir <- system.file(package = "hetid")

  if (pkg_dir == "") {
    # Development mode - package not installed
    file.path("inst", "extdata")
  } else {
    # Installed package
    file.path(pkg_dir, "extdata")
  }
}

#' Get Data File Path
#'
#' Constructs full path to a data file in the package data directory.
#' Provides consistent path resolution.
#'
#' @param filename Character string with filename (including extension)
#'
#' @return Character string with full path to data file
#' @keywords internal
get_data_file_path <- function(filename) {
  if (!is.character(filename) || length(filename) != 1) {
    stop("filename must be a single character string")
  }

  file.path(get_package_data_dir(), filename)
}

#' Check Data File Exists
#'
#' Checks if a data file exists in the package data directory.
#' Useful for workflows that depend on external data.
#'
#' @param filename Character string with filename (including extension)
#'
#' @return Logical indicating whether file exists
#' @keywords internal
check_data_file_exists <- function(filename) {
  file_path <- get_data_file_path(filename)
  file.exists(file_path)
}

#' Get ACM Data File Path
#'
#' Specialized function for ACM term premia data file path.
#' Provides path for the specific dataset.
#'
#' @return Character string with full path to ACM data file
#' @keywords internal
get_acm_data_path <- function() {
  get_data_file_path("ACMTermPremium.csv")
}

#' Validate Data Directory
#'
#' Ensures the data directory exists and is accessible.
#' Creates directory if needed.
#'
#' @param create_if_missing Logical, whether to create directory if it doesn't exist
#'
#' @return Invisible TRUE if successful, stops with error if validation fails
#' @keywords internal
validate_data_directory <- function(create_if_missing = TRUE) {
  data_dir <- get_package_data_dir()

  if (!dir.exists(data_dir)) {
    if (create_if_missing) {
      tryCatch(
        {
          dir.create(data_dir, recursive = TRUE)
        },
        error = function(e) {
          stop("Cannot create data directory: ", data_dir, "\nError: ", e$message)
        }
      )
    } else {
      stop("Data directory does not exist: ", data_dir)
    }
  }

  # Check if directory is writable
  if (!file.access(data_dir, mode = 2) == 0) {
    warning("Data directory may not be writable: ", data_dir)
  }

  invisible(TRUE)
}
