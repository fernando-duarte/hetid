#' @param release Optional GitHub release tag without whitespace or control
#'   characters. Supply together with
#'   \code{expected_sha256}; both default to NULL for the existing unpinned behavior.
#'   Pins are unavailable for \code{source = "nyfed"}.
#' @param expected_sha256 Expected 64-character hexadecimal sha256 digest of the
#'   compressed release asset, obtained independently by the caller.
#' @details
#' An explicit release and digest select an isolated snapshot in the per-user
#' data directory, keyed by release, digest, and frequency. Pinned reads verify
#' the digest, provenance, schema, and dates and never fall back to the latest
#' release, an unpinned cache, or the bundled asset. Pinned downloads may copy an
#' existing unpinned cache or bundled asset whose digest matches, verifying the bytes before
#' publication. This leaves the original asset unchanged.
#'
#' Data and provenance are published together. The sidecar at the path returned by
#' \code{download_term_premia()} plus \code{.meta} records the release, digest,
#' frequency, URL, UTC timestamp, and whether bytes were downloaded or copied
#' from an existing verified asset.
#' The URL is informational provenance; release, digest, and frequency define identity.
#' A failed download leaves existing snapshots unchanged. With a pin, calling
#' \code{download_term_premia(force = TRUE, ...)} downloads and validates fresh bytes
#' but retains an existing valid snapshot of the identical bytes. A corrupt snapshot
#' fails closed even with force; remove the named snapshot directory explicitly before retrying.
#' A terminated download process may leave an unused staging directory in the cache;
#' staging directories are never read as snapshots.
#' Pinned assets require at least one row and fully parseable, nonmissing dates.
#' Monthly and quarterly extraction use the monthly asset and its digest; daily
#' extraction uses the daily asset and its digest.
