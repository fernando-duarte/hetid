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
#' missing or empty result -- into a structured error.
#'
#' @param url Source URL
#' @param destfile Destination path
#' @param quiet Logical, suppress progress output
#' @param what Human label for error messages
#' @return Invisibly returns \code{destfile}
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
#' Replaces \code{dest} with \code{temp} via an unlink-then-rename so a
#' partial write never half-overwrites the cache. The target is cleared
#' first because rename-onto-existing fails on Windows; a failed rename
#' raises a structured error naming \code{what}.
#'
#' @param temp Source temp-file path (in the same directory as
#'   \code{dest} so the rename stays one filesystem operation)
#' @param dest Destination cache path
#' @param what Human label for the error message
#' @return Invisibly returns \code{dest}
#' @keywords internal
atomic_replace <- function(temp, dest, what) {
  unlink(dest)
  if (!file.rename(temp, dest)) {
    stop_hetid(paste0("Could not move ", what, " into ", dest))
  }
  invisible(dest)
}
