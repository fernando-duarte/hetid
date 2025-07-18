#' @keywords internal
#' @importFrom utils download.file read.csv write.csv
#' @importFrom stats aggregate
"_PACKAGE"

#' hetid: Identification Through Heteroskedasticity for VFCI
#'
#' @description
#' The hetid package provides tools for identification through heteroskedasticity
#' following Lewbel (2012), with a focus on applications to the Volatility
#' Financial Conditions Index (VFCI).
#'
#' @section Key Features:
#' \itemize{
#'   \item Download and process Federal Reserve economic data
#'   \item Extract and analyze ACM term structure data
#'   \item Implement heteroskedasticity-based identification methods
#'   \item Support for VFCI analysis
#' }
#'
#' @section Data Sources:
#' The package provides access to three main data sources:
#' \describe{
#'   \item{ACM Term Premia}{Monthly term structure data from Adrian, Crump, and
#'     Moench (2013), including yields, term premia, and risk-neutral yields for
#'     1-10 year maturities. See \code{\link{acm_data}} for details.}
#'   \item{Fed Yield Curve (Svensson)}{Daily Svensson yield curve parameters
#'     (feds200628). Download with \code{\link{download_yield_curve}}.}
#'   \item{Fed Yield Curve (Smoothed)}{Daily smoothed yield curve data
#'     (feds200533). Download with \code{\link{download_yield_curve}}.}
#' }
#'
#' @section Getting Started:
#' \preformatted{
#' # Extract ACM data for analysis
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(2, 5, 10)
#' )
#'
#' # Download Fed yield curve data
#' download_yield_curve("both")
#' svensson <- load_yield_curve("feds200628")
#' }
#'
#' @references
#' Adrian, T., Crump, R. K., and Moench, E. (2013). "Pricing the term structure
#' with linear regressions." Journal of Financial Economics, 110(1), 110-138.
#'
#' Lewbel, A. (2012). "Using heteroscedasticity to identify and estimate
#' mismeasured and endogenous regressor models." Journal of Business & Economic
#' Statistics, 30(1), 67-80.
#'
#' @name hetid-package
#' @aliases hetid

## usethis namespace: start
## usethis namespace: end
NULL
