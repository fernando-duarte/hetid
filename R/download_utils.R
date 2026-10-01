#' Shared Download Internals for the ACM Data Sources
#'
#' Fail-closed fetch and atomic cache-replace helpers shared by the
#' GitHub-release and NY Fed download paths.
#'
#' @name download_utils
#' @keywords internal
NULL

#' Fetch a URL to a File, Fail-Closed
#'
#' Wraps \code{download.file} with an explicit libcurl method (so the
#' release-redirect behavior does not depend on the environment) and
#' converts every failure mode -- error, warning, non-zero status,
#' missing or empty result -- into a \code{hetid_error} condition.
#'
#' The download writes to \code{destfile} in binary mode and may overwrite an
#' existing file. This helper does not remove a partial or empty file on failure;
#' the caller is responsible for cleanup.
#'
#' @param url Character scalar giving the source URL.
#' @param destfile Character scalar giving the destination file path.
#' @param quiet Logical scalar; \code{TRUE} suppresses progress output.
#' @param what Character scalar giving a human-readable label for error messages.
#' @return The character scalar \code{destfile}, invisibly, on success.
#' @keywords internal
fetch_url_to_file <- function(url, destfile, quiet, what) {
  status <- tryCatch(
    download.file(
      url = url, destfile = destfile, mode = "wb",
      quiet = quiet, method = "libcurl"
    ),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  ok <- identical(status, 0L) && file.exists(destfile) &&
    file.size(destfile) > 0
  if (!ok) {
    detail <- if (is.character(status)) paste0(" (", status, ")") else ""
    stop_hetid(paste0(
      "Failed to download ", what, " from ", url, detail
    ))
  }
  invisible(destfile)
}

#' Atomically Move a Temp File into the Cache
#'
#' Renames \code{temp} onto \code{dest} without first deleting the target.
#' A failed rename preserves the existing cache and raises a \code{hetid_error}
#' condition. Any warning from \code{file.rename} is also emitted.
#' Both paths must be on the same filesystem.
#'
#' On success, the file is available at \code{dest} and \code{temp} no longer
#' exists, provided the paths differ.
#'
#' @param temp Character scalar giving the source temporary-file path, in the same
#'   directory as \code{dest} so the rename stays one filesystem operation.
#' @param dest Character scalar giving the destination cache path.
#' @param what Character scalar giving a human-readable label for the error message.
#' @return The character scalar \code{dest}, invisibly, on success.
#' @keywords internal
atomic_replace <- function(temp, dest, what) {
  if (!file.rename(temp, dest)) {
    stop_hetid(paste0("Could not move ", what, " into ", dest))
  }
  invisible(dest)
}
