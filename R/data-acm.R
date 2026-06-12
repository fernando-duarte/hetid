#' ACM Term Structure Data Documentation
#'
#' @description
#' Documentation for the Adrian, Crump, and Moench (ACM) term structure dataset
#' available through the \code{load_term_premia()} and \code{extract_acm_data()}
#' functions. This dataset contains model-implied yields, term premia, and
#' risk-neutral yields for U.S. Treasury securities.
#'
#' @format The ACM data is stored as a gzipped CSV with 346 columns:
#' the date plus 115 maturities (6 to 120 months at one-month steps)
#' for each of three families:
#' \describe{
#'   \item{date}{The business-day observation date for each row in the series
#'   (originally 'DATE' in source data, ISO formatted, standardized to
#'   lowercase)}
#'   \item{ACMY01-ACMY10, ACMY006M-ACMY119M}{Model-implied zero-coupon
#'     Treasury yields from the ACM model. Whole-year maturities keep
#'     the official padded-year names (ACMY01 = 12 months, ...,
#'     ACMY10 = 120 months); sub-annual months use the three-digit
#'     month form (e.g. ACMY006M = 6 months)}
#'   \item{ACMTP01-ACMTP10, ACMTP006M-ACMTP119M}{ACM term premium
#'     estimates—the additional compensation investors require for
#'     bearing interest-rate risk—under the same dual naming}
#'   \item{ACMRNY01-ACMRNY10, ACMRNY006M-ACMRNY119M}{Risk-neutral yields
#'     (the expected average short-rate path) as implied by the ACM
#'     model, under the same dual naming}
#' }
#'
#' @details
#' ## Maturity Naming
#' Whole-year maturities carry the official two-digit year suffix
#' (01-10, i.e. 12 to 120 months); all other maturities carry a
#' three-digit month suffix plus "M" (006M-119M). Package-facing
#' columns from \code{extract_acm_data()} are always suffixed by the
#' maturity in months (y12, tp60, rny119, ...).
#'
#' ## Units
#' All yield, term-premium, and risk-neutral-yield values are expressed in
#' annualized percentage points (i.e., percent).
#'
#' ## Frequency
#' The dataset is monthly frequency (one entry per month), typically using
#' end-of-month observations. The NY Fed fallback source
#' (\code{source = "nyfed"}) provides only the annual-node maturities.
#'
#' ## Relationship Between Fields
#' By construction, for each maturity n:
#' \deqn{\text{ACMTP}_n = \text{ACMY}_n - \text{ACMRNY}_n}
#'
#' That is, the term premium is simply the difference between the model-implied
#' yield and the risk-neutral yield.
#'
#' @section Data Source:
#' The bundled file is the validated ACM reproduction published at
#' \url{https://github.com/fernando-duarte/ACM_term_premium} (release
#' \code{acm-term-premium-2026-06}, asset
#' \code{ACMTermPremium_reproduced_monthly_6m_120m.csv.gz}, sha256
#' \code{d95255a08200aadbf66dd33bdd54111e9f58bfab987a5683e93ec81c6b087e89}).
#' It reproduces the official NY Fed workbook to within 0.0026 basis
#' points at the annual nodes and extends the maturity grid to monthly
#' steps from 6 to 120 months: whole-year maturities keep the official
#' column names (\code{ACMY01}..\code{ACMY10}), sub-annual months use
#' names like \code{ACMY006M}. Dates are ISO (\code{YYYY-MM-DD}).
#' \code{download_term_premia()} refreshes the cache from the latest
#' release with sha256 verification; the official NY Fed workbook
#' remains available as the opt-in \code{source = "nyfed"} fallback
#' (annual maturities only). The methodology is Adrian, Crump, and
#' Moench (2013).
#'
#' @section Usage:
#' To access the raw ACM data:
#' \preformatted{
#' # Download the data (if not already available)
#' download_term_premia()
#'
#' # Load the raw data
#' acm_raw <- load_term_premia()
#'
#' # Or use the convenience function for processed data
#' acm_data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(2, 5, 10)
#' )
#' }
#'
#' @references
#' Adrian, T., Crump, R. K., and Moench, E. (2013). "Pricing the term structure
#' with linear regressions." Journal of Financial Economics, 110(1), 110-138.
#'
#' Federal Reserve Bank of New York. "Treasury Term Premia."
#' \url{https://www.newyorkfed.org/research/data_indicators/term-premia-tabs}
#'
#' @seealso
#' \code{\link{load_term_premia}} for loading the raw data
#' \code{\link{extract_acm_data}} for convenient data extraction
#' \code{\link{download_term_premia}} for downloading the latest data
#'
#' @name acm_data
#' @aliases ACM_data ACM acm term_premia
#' @keywords datasets
NULL
