#' GitHub-Release Download Internals for the ACM Data
#'
#' Digest-verified download of the monthly-maturity ACM reproduction
#' from the GitHub release. Every failure path is fail-closed: nothing
#' is cached unless the sha256 digest from the release API matches the
#' downloaded file.
#'
#' @name download_acm_github_internals
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

#' Expected sha256 Digest for the ACM Release Asset
#'
#' Reads the latest-release metadata from the GitHub API and extracts
#' the per-asset sha256 digest for the ACM data filename. Runtime
#' dependencies are base-only, so the digest is extracted with a
#' constrained regex over the asset's JSON segment rather than a JSON
#' parser; any ambiguity (zero or multiple matching assets, missing or
#' malformed digest) fails closed.
#'
#' @param quiet Logical, suppress progress output
#' @return Lower-case 64-character sha256 string
#' @keywords internal
acm_release_expected_sha256 <- function(quiet = FALSE) {
  json_file <- tempfile(pattern = "acm_release_", fileext = ".json")
  on.exit(unlink(json_file), add = TRUE)
  fetch_url_to_file(
    DATA_URLS$ACM_GITHUB_RELEASE_API, json_file,
    quiet = quiet, what = "release metadata"
  )

  json_text <- paste(readLines(json_file, warn = FALSE), collapse = "\n")
  json_chunks <- strsplit(json_text, '"name"[[:space:]]*:')[[1]]
  target_prefix <- paste0('"', HETID_CONSTANTS$ACM_DATA_FILENAME, '"')
  hits <- which(startsWith(trimws(json_chunks), target_prefix))
  if (length(hits) != 1) {
    stop_hetid(paste0(
      "Release metadata does not identify exactly one asset named ",
      HETID_CONSTANTS$ACM_DATA_FILENAME, " (found ", length(hits),
      "); refusing to download without a verifiable digest."
    ))
  }

  digest_pattern <-
    '"digest"[[:space:]]*:[[:space:]]*"sha256:[0-9a-fA-F]{64}"'
  digest_match <- regmatches(
    json_chunks[hits], regexpr(digest_pattern, json_chunks[hits])
  )
  if (length(digest_match) != 1) {
    stop_hetid(paste0(
      "Release metadata has no usable sha256 digest for ",
      HETID_CONSTANTS$ACM_DATA_FILENAME,
      "; refusing to download without a verifiable digest."
    ))
  }
  tolower(sub('^.*sha256:([0-9a-fA-F]{64})"$', "\\1", digest_match))
}

#' Download and Verify the ACM Data from the GitHub Release
#'
#' @param quiet Logical, suppress progress output
#' @return Invisibly returns the cached file path
#' @keywords internal
download_acm_github <- function(quiet = FALSE) {
  expected_sha <- acm_release_expected_sha256(quiet = quiet)
  cache_path <- get_acm_download_path("github")

  # Same directory as the cache so the final rename stays one
  # filesystem operation
  temp_gz <- tempfile(
    pattern = "acm_download_", tmpdir = dirname(cache_path),
    fileext = ".csv.gz"
  )
  on.exit(unlink(temp_gz), add = TRUE)

  if (!quiet) {
    message("Downloading ACM data from the GitHub release...")
  }
  fetch_url_to_file(
    DATA_URLS$ACM_GITHUB_CSV_GZ, temp_gz,
    quiet = quiet, what = "ACM data file"
  )

  actual_sha <- tolower(unname(tools::sha256sum(temp_gz)))
  if (is.na(actual_sha) || !identical(actual_sha, expected_sha)) {
    stop_hetid(paste0(
      "Checksum mismatch for downloaded ACM data: expected sha256 ",
      expected_sha, ", got ", actual_sha,
      ". The file was not cached; please retry."
    ))
  }

  # rename-onto-existing fails on Windows, so clear the target first
  unlink(cache_path)
  if (!file.rename(temp_gz, cache_path)) {
    stop_hetid(paste0(
      "Could not move the verified download into ", cache_path
    ))
  }

  # Provenance sidecar: makes cached-vs-bundled differences debuggable
  writeLines(
    c(
      paste0("sha256: ", actual_sha),
      paste0("source_url: ", DATA_URLS$ACM_GITHUB_CSV_GZ),
      paste0(
        "retrieved: ",
        format(Sys.time(), HETID_CONSTANTS$ISO_DATE_FORMAT)
      )
    ),
    paste0(cache_path, ".meta")
  )

  if (!quiet) {
    message("ACM data saved to: ", cache_path)
  }
  invisible(cache_path)
}
