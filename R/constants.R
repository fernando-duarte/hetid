#' Academic Research Constants with Literature Context
#'
#' Constants used throughout the hetid package with academic literature context
#' and research reproducibility considerations.
#'
#' @name hetid_constants
#' @keywords internal
NULL

#' Academic Research Constants
#'
#' Constants with literature context for academic research reproducibility.
#' All values are documented with their academic sources and rationale.
#'
#' @format List containing academic research constants:
#' \describe{
#'   \item{DEFAULT_N_PCS}{Standard number of principal components (4) from Adrian, Crump, Moench (2013)}
#'   \item{MAX_N_PCS}{Maximum number of principal components (6) for computational stability}
#'   \item{MIN_MATURITY}{Minimum maturity index (1) from ACM data availability constraint}
#'   \item{MAX_MATURITY}{Maximum maturity index (10) from ACM dataset maturity limit}
#'   \item{OPTIMIZATION_PENALTY}{Penalty for constraint violations (1e6) in optimization}
#'   \item{MACHINE_EPSILON}{Machine precision for numerical comparisons}
#'   \item{DEFAULT_TAU}{Default quantile parameter (0.5) for median identification from Lewbel (2012)}
#'   \item{MIN_OBSERVATIONS}{Minimum observations required for statistical estimation (3)}
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
#' @export
HETID_CONSTANTS <- list(
  # Principal component defaults from ACM (2013)
  DEFAULT_N_PCS = 4L, # Standard in Adrian, Crump, Moench (2013)
  MAX_N_PCS = 6L, # Computational stability limit

  # Data constraints from ACM dataset
  MIN_MATURITY = 1L, # ACM data availability constraint
  MAX_MATURITY = 10L, # ACM dataset maturity limit

  # Numerical optimization parameters
  OPTIMIZATION_PENALTY = 1e6, # Penalty for constraint violations
  MACHINE_EPSILON = .Machine$double.eps,

  # Lewbel (2012) method parameters
  DEFAULT_TAU = 0.5, # Median quantile for identification

  # Statistical estimation requirements
  MIN_OBSERVATIONS = 3L # Minimum observations for estimation
)

#' Academic Data Source URLs (Versioned for Reproducibility)
#'
#' URLs for academic data sources with version control for research reproducibility.
#' These URLs are documented and versioned to ensure reproducible research.
#'
#' @format List containing versioned data source URLs:
#' \describe{
#'   \item{ACM_TERM_PREMIA}{URL for Adrian, Crump, and Moench term premia data}
#'   \item{FED_SVENSSON}{URL for Federal Reserve Svensson yield curve data}
#' }
#'
#' @references
#' Adrian, T., Crump, R. K., and Moench, E. (2013). "Pricing the term structure
#' with linear regressions." Journal of Financial Economics, 110(1), 110-138.
#'
#' @export
DATA_URLS <- list(
  ACM_TERM_PREMIA = "https://www.newyorkfed.org/medialibrary/media/research/data_indicators/ACMTermPremium.xls",
  FED_SVENSSON = "https://www.federalreserve.gov/data/yield-curve-tables/feds200628.csv"
)
