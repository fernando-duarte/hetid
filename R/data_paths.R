#' Path Management Utilities
#'
#' Path resolution functions for package data. Bundled data ships
#' read-only in the installed package's extdata directory; downloaded
#' copies live in the per-user data directory from
#' \code{\link[tools:R_user_dir]{tools::R_user_dir}} with \code{which = "data"}.
#'
#' @name data_paths
#' @keywords internal
NULL

#' Get Bundled Package Data Directory
#'
#' Returns the extdata path in the package located by \code{system.file()}.
#' Treat bundled data as read-only.
#'
#' @return A character scalar giving the bundled data directory path.
#' @keywords internal
get_package_data_dir <- function() {
  system.file("extdata", package = "hetid")
}

#' Get User Data Directory
#'
#' Returns the per-user cache directory for downloaded data files.
#' Downloads must never write into the installed package library, so
#' all writes target this directory instead.
#'
#' @param create A non-missing logical scalar. If \code{TRUE}, create the
#'   directory recursively if missing; \code{FALSE} (default) only resolves it.
#'
#' @details Uses \code{\link[tools:R_user_dir]{tools::R_user_dir}} with
#' \code{which = "data"}, including its \code{R_USER_DATA_DIR} override.
#' Failure to create the directory raises a \code{hetid_error} condition.
#' @return A character scalar giving the user data directory path. With
#'   \code{create = FALSE}, the directory need not exist.
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
#' @param filename A non-empty, non-missing character scalar giving the
#'   filename, including its extension.
#'
#' @details Invalid filenames raise a \code{hetid_error_bad_argument} condition.
#' No directory or file is created.
#' @return A character scalar giving the data file path. The bundled fallback
#'   is returned even if the file does not exist.
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

#' Resolve the ACM Asset Filename
#'
#' Single source of the (source, frequency) to filename mapping and of
#' the rule that daily data is GitHub-only. Both path resolvers below
#' derive from it, so the read and write paths cannot desync. Callers
#' pass \code{match.arg()}-validated values.
#'
#' @param source A character scalar: \code{"auto"}, \code{"github"}, or
#'   \code{"nyfed"}, already validated by the caller.
#' @param frequency A character scalar: \code{"monthly"} or \code{"daily"},
#'   already validated by the caller.
#' @details The \code{"nyfed"}/\code{"daily"} combination raises a
#' \code{hetid_error_bad_argument} condition. \code{"auto"} uses the GitHub asset.
#' @return A character scalar giving the asset filename from
#'   \code{HETID_CONSTANTS}.
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
#' @param source A character scalar. \code{"auto"} (default) and
#'   \code{"github"} resolve the GitHub user cache then the bundled file;
#'   \code{"nyfed"} resolves the NY Fed cache path only.
#' @param frequency A character scalar: \code{"monthly"} (default) or
#'   \code{"daily"} (GitHub-only, user cache only).
#' @details No file or directory is created. The \code{"nyfed"}/\code{"daily"}
#' combination raises a \code{hetid_error_bad_argument} condition.
#' @return A character scalar giving the ACM data file path. The file need
#'   not exist; callers check existence.
#' @keywords internal
get_acm_data_path <- function(source = c("auto", "github", "nyfed"),
                              frequency = c("monthly", "daily")) {
  source <- match.arg(source)
  frequency <- match.arg(frequency)
  filename <- acm_asset_filename(source, frequency)
  if (source == "nyfed" || frequency == "daily") {
    return(file.path(get_user_data_dir(), filename))
  }
  get_data_file_path(filename)
}

#' Check ACM Data Availability
#'
#' Reports whether the resolved ACM path exists without reading its contents.
#'
#' @param source A character scalar: \code{"auto"} (default), \code{"github"},
#'   or \code{"nyfed"}, passed to \code{\link{get_acm_data_path}}.
#' @param frequency A character scalar: \code{"monthly"} (default) or
#'   \code{"daily"}, passed to \code{\link{get_acm_data_path}}.
#' @return A logical scalar: \code{TRUE} if the resolved path exists,
#'   \code{FALSE} otherwise. Resolution errors propagate.
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
#' @param source A character scalar: \code{"github"} (default) or \code{"nyfed"}.
#' @param frequency A character scalar: \code{"monthly"} (default) or
#'   \code{"daily"} (GitHub-only).
#' @details No data file is created. The \code{"nyfed"}/\code{"daily"}
#' combination raises a \code{hetid_error_bad_argument} condition; directory
#' creation failures raise a \code{hetid_error} condition.
#' @return A character scalar giving the ACM download destination. Its parent
#'   directory exists, but writability is not checked.
#' @keywords internal
get_acm_download_path <- function(source = c("github", "nyfed"),
                                  frequency = c("monthly", "daily")) {
  source <- match.arg(source)
  frequency <- match.arg(frequency)
  file.path(
    get_user_data_dir(create = TRUE), acm_asset_filename(source, frequency)
  )
}
