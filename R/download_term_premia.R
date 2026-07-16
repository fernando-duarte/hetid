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
#'
#' @param source Data source: \code{"github"} (default,
#'   digest-verified) or \code{"nyfed"} (official workbook fallback,
#'   annual maturities only).
#' @param force Logical. If TRUE, forces re-download even if data exists.
#' @param quiet Logical. If TRUE, suppresses download progress messages.
#' @param frequency Data frequency: \code{"monthly"} (default) or
#'   \code{"daily"} (the ~40 MB business-day asset; GitHub source only).
#'
#' @return Invisibly returns the path to the saved data file.
#' @export
#'
#' @examplesIf interactive()
#' # Download the data (only if not already present)
#' download_term_premia()
#'
#' # Force re-download from the GitHub release
#' download_term_premia(force = TRUE)
#'
#' # Opt into the NY Fed workbook fallback
#' download_term_premia(source = "nyfed")
#'
#' # Download the daily (business-day) series
#' download_term_premia(frequency = "daily")
#'
#' @references
#' Adrian, T., Crump, R. K., and Moench, E. (2013).
#' "Pricing the term structure with linear regressions."
#' Journal of Financial Economics, 110(1), 110-138.
#'
download_term_premia <- function(source = c("github", "nyfed"),
                                 force = FALSE, quiet = FALSE,
                                 frequency = c("monthly", "daily")) {
  source <- match.arg(source)
  frequency <- match.arg(frequency)

  # Path resolution also rejects nyfed + daily, even when force = TRUE
  existing <- get_acm_data_path(source, frequency)
  if (!force && file.exists(existing)) {
    if (!quiet) {
      message(
        "Term premia data already exists. Use force = TRUE to re-download."
      )
    }
    return(invisible(existing))
  }

  switch(source,
    github = download_acm_github(quiet = quiet, frequency = frequency),
    nyfed = download_acm_nyfed(quiet = quiet)
  )
}
