#' Package Constants
#'
#' @description
#' Constants used for computations and data processing.
#' All values are documented with their sources.
#'
#' @format List containing package constants:
#' \describe{
#'   \item{DEFAULT_N_PCS}{Standard number of principal components (4)}
#'   \item{MAX_N_PCS}{Maximum principal components (6)}
#'   \item{MIN_MATURITY}{Minimum maturity index (1)}
#'   \item{MAX_MATURITY}{Maximum maturity index (10)}
#'   \item{EFFECTIVE_MAX_MATURITY}{Max maturity for functions
#'     needing i+1 (9)}
#'   \item{MACHINE_EPSILON}{Machine precision}
#'   \item{MATRIX_SYMMETRY_TOL}{Tolerance for matrix symmetry
#'     checks}
#'   \item{MIN_OBSERVATIONS}{Minimum observations for estimation}
#'   \item{PERCENT_TO_DECIMAL}{Percentage to decimal divisor}
#'   \item{MONTHS_PER_QUARTER}{Calendar months per quarter (3)}
#'   \item{ACM_DATE_FORMAT}{Date format in ACM files}
#'   \item{ISO_DATE_FORMAT}{Standard ISO date format}
#'   \item{YEAR_FORMAT}{Year extraction format}
#'   \item{MONTH_FORMAT}{Month extraction format}
#'   \item{CONSUMPTION_GROWTH_COL}{Consumption growth column name}
#'   \item{PC_PREFIX}{Prefix for principal component columns}
#'   \item{MATURITY_PREFIX}{Prefix for maturity label columns}
#'   \item{ACM_DATA_FILENAME}{ACM data CSV filename}
#'   \item{BUNDLED_VARIABLES_DATASET}{Bundled dataset name}
#'   \item{COL_FORMAT_PADDED}{Padded column name format}
#'   \item{COL_FORMAT_SIMPLE}{Simple column name format}
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
  # Principal component defaults
  DEFAULT_N_PCS = 4L, # Default from Adrian, Crump, Moench (2013)
  MAX_N_PCS = 6L, # Maximum for stability

  # Data constraints
  MIN_MATURITY = 1L, # Minimum available maturity
  MAX_MATURITY = 10L, # Maximum available maturity
  EFFECTIVE_MAX_MATURITY = 9L, # For functions needing i+1

  # Numerical parameters
  MACHINE_EPSILON = .Machine$double.eps,
  MATRIX_SYMMETRY_TOL = 1e-10, # Matrix symmetry check tol
  PERCENT_TO_DECIMAL = 100, # Divisor for percentage conversion

  # Statistical requirements
  MIN_OBSERVATIONS = 3L, # Minimum observations needed

  # Calendar
  MONTHS_PER_QUARTER = 3L,

  # Date formats
  ACM_DATE_FORMAT = "%d-%b-%Y", # ACM data date format
  ISO_DATE_FORMAT = "%Y-%m-%d", # ISO standard date format
  YEAR_FORMAT = "%Y", # Year extraction format
  MONTH_FORMAT = "%m", # Month extraction format

  # Column names
  CONSUMPTION_GROWTH_COL = "gr1.pcecc96",
  PC_PREFIX = "pc",
  MATURITY_PREFIX = "maturity_",

  # Data identity
  ACM_DATA_FILENAME = "ACMTermPremium.csv",
  BUNDLED_VARIABLES_DATASET = "variables",

  # Column format patterns
  COL_FORMAT_PADDED = "%s%02d", # e.g., ACMY01
  COL_FORMAT_SIMPLE = "%s%d" # e.g., y1
)

#' Data Source URLs
#'
#' URLs for external data sources.
#' These URLs are documented for data access.
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
  ACM_TERM_PREMIA =
    "https://www.newyorkfed.org/medialibrary/media/research/data_indicators/ACMTermPremium.xls",
  FED_SVENSSON =
    "https://www.federalreserve.gov/data/yield-curve-tables/feds200628.csv"
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
#' @export
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
