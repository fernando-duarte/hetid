#' Download ACM Term Premia Data
#'
#' Downloads the Adrian, Crump, and Moench (ACM) term-structure data
#' into the per-user data directory
#' (\code{tools::R_user_dir("hetid", "data")}). The bundled copy shipped
#' with the package is never modified.
#'
#' The default \code{"github"} source fetches the monthly-maturity ACM
#' replication from the fernando-duarte/ACM_term_premium release and
#' verifies the file against the release's per-asset sha256 digest
#' before caching it; any mismatch fails without caching. Pass
#' \code{frequency = "daily"} for the release's business-day asset. The
#' opt-in \code{"nyfed"} source downloads the official NY Fed workbook
#' instead (annual maturities only, monthly frequency only) and requires
#' the \pkg{readxl} package.
#'
#' Unlike \code{\link{load_term_premia}}, there is no \code{"auto"}
#' source here: a download is always source-specific. The bundled copy
#' counts as available for the github source, but never suppresses an
#' explicit nyfed download.
#' Unpinned existing files are returned without rechecking their contents.
#'
#' @param source Character string specifying \code{"github"} (default,
#'   digest-verified) or \code{"nyfed"} (official workbook fallback,
#'   annual maturities only).
#' @param force Single nonmissing logical. If \code{TRUE}, forces re-download
#'   even if data exists. Defaults to \code{FALSE}.
#' @param quiet Single nonmissing logical. If \code{TRUE}, suppresses download
#'   progress messages. Defaults to \code{FALSE}.
#' @param frequency Character string specifying \code{"monthly"} (default) or
#'   \code{"daily"} (the ~40 MB business-day asset; GitHub source only).
#'
#' @template acm-pin
#'
#' @return Invisibly returns a single character string giving the data file
#'   path. With \code{force = FALSE}, this may be an existing cached file or
#'   the bundled monthly GitHub asset, without a new download.
#' @export
#'
#' @examplesIf interactive()
#' local({
#'   # Keep downloaded files out of the normal user cache
#'   data_dir <- tempfile("hetid-data-")
#'   old_dir <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
#'   on.exit({
#'     if (is.na(old_dir)) {
#'       Sys.unsetenv("R_USER_DATA_DIR")
#'     } else {
#'       Sys.setenv(R_USER_DATA_DIR = old_dir)
#'     }
#'     unlink(data_dir, recursive = TRUE)
#'   })
#'   Sys.setenv(R_USER_DATA_DIR = data_dir)
#'
#'   # The bundled monthly asset can satisfy an unforced request
#'   path <- download_term_premia()
#'   file.exists(path)
#'
#'   # Force re-download from the GitHub release
#'   download_term_premia(force = TRUE)
#'
#'   # The NY Fed workbook requires the optional readxl package
#'   if (requireNamespace("readxl", quietly = TRUE)) {
#'     download_term_premia(source = "nyfed")
#'   }
#'
#'   # Download the daily (business-day) series
#'   download_term_premia(frequency = "daily")
#' })
#'
#' @references
#' Adrian, T., Crump, R. K., and Moench, E. (2013).
#' "Pricing the term structure with linear regressions."
#' Journal of Financial Economics, 110(1), 110-138.
#'
download_term_premia <- function(source = c("github", "nyfed"),
                                 force = FALSE, quiet = FALSE,
                                 frequency = c("monthly", "daily"),
                                 release = NULL, expected_sha256 = NULL) {
  source <- match.arg(source)
  frequency <- match.arg(frequency)
  assert_flag(force, "force")
  assert_flag(quiet, "quiet")
  pin <- acm_pin(release, expected_sha256, source, frequency)
  if (!is.null(pin)) {
    return(download_acm_pin(pin, force, quiet))
  }

  # Path resolution also rejects nyfed + daily, even when force = TRUE
  existing <- get_acm_data_path(source, frequency)
  if (!force && file.exists(existing)) {
    if (dir.exists(existing)) {
      stop_hetid(paste0("Term premia data path is a directory: ", existing))
    }
    if (!quiet) {
      message(
        "Term premia data already exists. Use force = TRUE to re-download."
      )
    }
    return(invisible(existing))
  }

  switch(source,
    github = download_acm_github(quiet = quiet, frequency = frequency),
    nyfed = download_acm_nyfed(quiet = quiet),
    stop_bad_argument(
      paste0("source has no download handler: ", source),
      arg = "source"
    )
  )
}
