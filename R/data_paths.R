#' Path Management Utilities
#'
#' Path resolution functions for package data. Bundled data ships
#' read-only in the installed package's extdata directory; downloaded
#' copies live in the per-user cache from \code{tools::R_user_dir()}.
#'
#' @name data_paths
#' @keywords internal
NULL

#' Get Bundled Package Data Directory
#'
#' Returns the read-only extdata directory shipped with the package,
#' for both development and installed package environments.
#'
#' @return Character string with path to the bundled data directory
#' @keywords internal
get_package_data_dir <- function() {
  # Path resolution for package data
  pkg_dir <- system.file(package = "hetid")

  if (pkg_dir == "") {
    # nocov start
    # Dev mode only — unreachable under pkgload::load_all()
    file.path("inst", "extdata")
    # nocov end
  } else {
    # Installed package
    file.path(pkg_dir, "extdata")
  }
}

#' Get User Data Directory
#'
#' Returns the per-user cache directory for downloaded data files.
#' Downloads must never write into the installed package library, so
#' all writes target this directory instead.
#'
#' @param create Logical, whether to create the directory if missing
#'
#' @return Character string with path to the user data directory
#' @keywords internal
get_user_data_dir <- function(create = FALSE) {
  user_dir <- tools::R_user_dir("hetid", which = "data")

  if (create && !dir.exists(user_dir)) {
    created <- dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)
    if (!created && !dir.exists(user_dir)) {
      stop_hetid(paste0(
        "Cannot create user data directory: ", user_dir
      ))
    }
  }

  user_dir
}

#' Get Data File Path
#'
#' Resolves a data filename to the per-user downloaded copy when one
#' exists, falling back to the read-only bundled copy otherwise.
#'
#' @param filename Character string with filename (including extension)
#'
#' @return Character string with full path to data file
#' @keywords internal
get_data_file_path <- function(filename) {
  if (!is.character(filename) || length(filename) != 1) {
    stop_bad_argument(
      "filename must be a single character string",
      arg = "filename"
    )
  }

  user_path <- file.path(get_user_data_dir(), filename)
  if (file.exists(user_path)) {
    return(user_path)
  }

  file.path(get_package_data_dir(), filename)
}

#' Check Data File Exists
#'
#' Checks if a data file exists in the user cache or the bundled
#' package data directory.
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
#' Specialized function for the ACM term premia data file path,
#' resolved with user-cache preference and bundled fallback.
#'
#' @return Character string with full path to ACM data file
#' @keywords internal
get_acm_data_path <- function() {
  get_data_file_path(HETID_CONSTANTS$ACM_DATA_FILENAME)
}

#' Get Writable ACM Download Path
#'
#' Returns the user-cache path where downloads are written, creating
#' the directory on demand. The bundled copy is never overwritten.
#'
#' @return Character string with the writable ACM data file path
#' @keywords internal
get_acm_download_path <- function() {
  file.path(
    get_user_data_dir(create = TRUE),
    HETID_CONSTANTS$ACM_DATA_FILENAME
  )
}
