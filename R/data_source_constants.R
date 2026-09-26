# Release-asset download URLs are composed as prefix + asset filename
ACM_RELEASE_URL <- "https://github.com/fernando-duarte/ACM_term_premium/releases/"
ACM_RELEASE_DOWNLOAD_PREFIX <- paste0(ACM_RELEASE_URL, "latest/download/")

#' Data Source URLs
#'
#' URLs for external data sources.
#' These URLs are documented for data access. The release-asset
#' download URLs are composed at the call site from
#' \code{ACM_RELEASE_DOWNLOAD_PREFIX} plus the \code{HETID_CONSTANTS}
#' asset filenames.
#'
#' @format List containing versioned data source URLs:
#' \describe{
#'   \item{ACM_GITHUB_RELEASE_API}{GitHub API endpoint exposing the
#'     per-asset sha256 digests used to verify downloads}
#'   \item{ACM_NYFED_XLS}{URL for the official NY Fed ACM workbook
#'     (opt-in fallback source, annual maturities only)}
#' }
#'
#' @references
#' Adrian, T., Crump, R. K., and Moench, E. (2013). "Pricing the term structure
#' with linear regressions." Journal of Financial Economics, 110(1), 110-138.
#'
#' @keywords internal
DATA_URLS <- list(
  ACM_GITHUB_RELEASE_API = paste0(
    "https://api.github.com/repos/fernando-duarte/",
    "ACM_term_premium/releases/latest"
  ),
  ACM_NYFED_XLS =
    "https://www.newyorkfed.org/medialibrary/media/research/data_indicators/ACMTermPremium.xls"
)

#' ACM Data Schema
#'
#' Maps extraction types to ACM column-name prefixes.
#' Single source of truth for the ACM naming convention.
#'
#' @format Named list. Each element contains:
#' \describe{
#'   \item{prefix_old}{Padded prefix in raw ACM data}
#'   \item{prefix_new}{Short prefix used in package}
#' }
#'
#' @keywords internal
HETID_ACM_SCHEMA <- list(
  yields = list(
    prefix_old = "ACMY", prefix_new = "y"
  ),
  term_premia = list(
    prefix_old = "ACMTP", prefix_new = "tp"
  ),
  risk_neutral_yields = list(
    prefix_old = "ACMRNY", prefix_new = "rny"
  )
)
