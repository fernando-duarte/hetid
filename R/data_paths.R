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
  pkg_dir <- system.file(package = "hetid")

  if (pkg_dir == "") {
    # nocov start
    file.path("inst", "extdata")
    # nocov end
  } else {
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
  if (!is.character(filename) || length(filename) != 1 ||
    is.na(filename) || !nzchar(filename)) {
    stop_bad_argument(
      "filename must be a single character string that is not missing or empty",
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

#' Resolve the ACM Asset Filename
#'
#' Single source of the (source, frequency) to filename mapping and of
#' the rule that daily data is GitHub-only. Both path resolvers below
#' derive from it, so the read and write paths cannot desync. Callers
#' pass \code{match.arg()}-validated values.
#'
#' @param source Data source: "auto", "github", or "nyfed"
#' @param frequency Data frequency: "monthly" or "daily"
#' @return Character string with the asset filename
#' @keywords internal
acm_asset_filename <- function(source, frequency) {
  if (frequency == "daily") {
    if (source == "nyfed") {
      stop_bad_argument(
        paste0(
          "Daily ACM data is only available from the GitHub source; ",
          "use source = \"github\""
        ),
        arg = "frequency"
      )
    }
    return(HETID_CONSTANTS$ACM_DAILY_DATA_FILENAME)
  }
  if (source == "nyfed") {
    return(HETID_CONSTANTS$ACM_NYFED_FILENAME)
  }
  HETID_CONSTANTS$ACM_DATA_FILENAME
}

#' Get ACM Data File Path
#'
#' Resolves the ACM data file for a source. The default GitHub-family
#' resolution prefers the per-user downloaded copy and falls back to
#' the bundled copy; the NY Fed fallback source resolves only its own
#' cache file and is never loaded implicitly. The daily-frequency asset
#' is GitHub-only and cache-only: it has no bundled copy, and the
#' bundled monthly file never satisfies a daily request.
#'
#' @param source Data source: "auto" and "github" resolve the GitHub
#'   user cache then the bundled file; "nyfed" resolves the NY Fed
#'   cache path only
#' @param frequency "monthly" (default) or "daily" (GitHub-only, user
#'   cache only)
#' @return Character string with full path to the ACM data file (the
#'   nyfed and daily paths may not exist; callers check existence)
#' @keywords internal
get_acm_data_path <- function(source = c("auto", "github", "nyfed"),
                              frequency = c("monthly", "daily")) {
  source <- match.arg(source)
  frequency <- match.arg(frequency)
  filename <- acm_asset_filename(source, frequency)
  # Only the monthly GitHub asset ships a bundled read-only fallback
  if (source == "nyfed" || frequency == "daily") {
    return(file.path(get_user_data_dir(), filename))
  }
  get_data_file_path(filename)
}

#' Check ACM Data Availability
#'
#' @param source Data source passed to \code{get_acm_data_path}
#' @param frequency Data frequency passed to \code{get_acm_data_path}
#' @return Logical indicating whether the resolved file exists
#' @keywords internal
acm_data_available <- function(source = c("auto", "github", "nyfed"),
                               frequency = c("monthly", "daily")) {
  file.exists(get_acm_data_path(source, frequency))
}

#' Get Writable ACM Download Path
#'
#' Returns the per-source user-cache path where downloads are written,
#' creating the directory on demand. The bundled copy is never
#' overwritten.
#'
#' @param source Download source: "github" or "nyfed"
#' @param frequency "monthly" (default) or "daily" (github only)
#' @return Character string with the writable ACM data file path
#' @keywords internal
get_acm_download_path <- function(source = c("github", "nyfed"),
                                  frequency = c("monthly", "daily")) {
  source <- match.arg(source)
  frequency <- match.arg(frequency)
  file.path(
    get_user_data_dir(create = TRUE), acm_asset_filename(source, frequency)
  )
}
