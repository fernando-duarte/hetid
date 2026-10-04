#' GitHub-Release Download Internals for the ACM Data
#'
#' Digest-verified download of the ACM replication (the monthly- or
#' daily-frequency asset, both on the monthly maturity grid) from the
#' GitHub release. Every failure path is fail-closed: nothing
#' is cached unless the sha256 digest from the release API matches the
#' downloaded file.
#'
#' @name download_acm_github_internals
#' @keywords internal
NULL

#' Expected sha256 Digest for the ACM Release Asset
#'
#' Reads the latest-release metadata from the GitHub API and extracts
#' the per-asset sha256 digest for the ACM data filename. Runtime
#' dependencies are base-only, so the digest is extracted with a
#' constrained regex over the asset's JSON segment rather than a JSON
#' parser; any ambiguity (zero or multiple matching assets, missing or
#' malformed digest) fails closed.
#'
#' @param quiet Logical scalar. If \code{TRUE}, suppresses download progress
#'   output. Defaults to \code{FALSE}.
#' @param filename Character string giving the release asset filename whose
#'   digest to extract.
#' @return A character string containing the lower-case, 64-character sha256
#'   digest. Missing or unusable release metadata raises a \code{hetid_error}.
#' @keywords internal
acm_release_expected_sha256 <- function(quiet = FALSE, filename) {
  json_file <- tempfile(pattern = "acm_release_", fileext = ".json")
  on.exit(unlink(json_file), add = TRUE)
  fetch_url_to_file(
    DATA_URLS$ACM_GITHUB_RELEASE_API, json_file,
    quiet = quiet, what = "release metadata"
  )

  json_text <- paste(readLines(json_file, warn = FALSE), collapse = "\n")
  json_chunks <- strsplit(json_text, '"name"[[:space:]]*:')[[1]]
  target_prefix <- paste0('"', filename, '"')
  hits <- which(startsWith(trimws(json_chunks), target_prefix))
  if (length(hits) != 1) {
    stop_hetid(paste0(
      "Release metadata does not identify exactly one asset named ",
      filename, " (found ", length(hits),
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
      filename,
      "; refusing to download without a verifiable digest."
    ))
  }
  tolower(sub('^.*sha256:([0-9a-fA-F]{64})"$', "\\1", digest_match))
}

#' Download and Verify the ACM Data from the GitHub Release
#'
#' Downloads the selected release asset, verifies its sha256 digest, and saves
#' it in the per-user data directory, replacing an existing cached copy.
#'
#' The data directory is created if needed.
#'
#' @param quiet Logical scalar. If \code{TRUE}, suppresses download progress
#'   output and status messages. Defaults to \code{FALSE}.
#' @param frequency Character string selecting the observation frequency:
#'   \code{"monthly"} (default) or \code{"daily"}. Both assets use the monthly
#'   maturity grid.
#' @return Invisibly returns a character string giving the cached file path.
#'   Download, digest-verification, or cache-replacement failures raise a
#'   \code{hetid_error}.
#' @keywords internal
download_acm_github <- function(quiet = FALSE,
                                frequency = c("monthly", "daily")) {
  frequency <- match.arg(frequency)
  asset_filename <- acm_asset_filename("github", frequency)
  asset_url <- paste0(ACM_RELEASE_DOWNLOAD_PREFIX, asset_filename)
  expected_sha <- acm_release_expected_sha256(
    quiet = quiet, filename = asset_filename
  )
  cache_path <- get_acm_download_path("github", frequency)

  temp_gz <- tempfile(
    pattern = "acm_download_", tmpdir = dirname(cache_path),
    fileext = ".csv.gz"
  )
  on.exit(unlink(temp_gz), add = TRUE)

  if (!quiet) {
    message("Downloading ACM data from the GitHub release...")
  }
  fetch_url_to_file(
    asset_url, temp_gz,
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

  atomic_replace(temp_gz, cache_path, "the verified download")

  if (!quiet) {
    message("ACM data saved to: ", cache_path)
  }
  invisible(cache_path)
}
