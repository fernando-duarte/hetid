#' Package Constants
#'
#' Constants used for computations and data processing.
#' All values are documented with their sources.
#'
#' @format List containing package constants:
#' \describe{
#'   \item{DEFAULT_N_PCS}{Standard number of principal components (4)
#'   extracted from financial asset returns}
#'   \item{MAX_N_PCS}{Maximum number of principal components (6) for computational stability}
#'   \item{MIN_MATURITY}{Minimum maturity index (1) from ACM data availability constraint}
#'   \item{MAX_MATURITY}{Maximum maturity index (10) from ACM dataset maturity limit}
#'   \item{MACHINE_EPSILON}{Machine precision for numerical comparisons}
#'   \item{MIN_OBSERVATIONS}{Minimum observations required for statistical estimation (3)}
#'   \item{PERCENT_TO_DECIMAL}{Divisor for percentage to decimal conversion (100)}
#'   \item{ACM_DATE_FORMAT}{Date format used in ACM data files ("\%d-\%b-\%Y")}
#'   \item{ISO_DATE_FORMAT}{Standard ISO date format ("\%Y-\%m-\%d")}
#'   \item{YEAR_FORMAT}{Format for extracting year ("\%Y")}
#'   \item{MONTH_FORMAT}{Format for extracting month ("\%m")}
#'   \item{CONSUMPTION_GROWTH_COL}{Column name for consumption growth ("gr1.pcecc96")}
#'   \item{COL_FORMAT_PADDED}{Format pattern for padded column names ("\%s\%02d")}
#'   \item{COL_FORMAT_SIMPLE}{Format pattern for simple column names ("\%s\%d")}
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

  # Numerical parameters
  MACHINE_EPSILON = .Machine$double.eps,
  PERCENT_TO_DECIMAL = 100, # Divisor for percentage conversion

  # Statistical requirements
  MIN_OBSERVATIONS = 3L, # Minimum observations needed

  # Date formats
  ACM_DATE_FORMAT = "%d-%b-%Y", # ACM data date format
  ISO_DATE_FORMAT = "%Y-%m-%d", # ISO standard date format
  YEAR_FORMAT = "%Y", # Year extraction format
  MONTH_FORMAT = "%m", # Month extraction format

  # Column names
  CONSUMPTION_GROWTH_COL = "gr1.pcecc96", # Consumption growth variable

  # Column format patterns
  COL_FORMAT_PADDED = "%s%02d", # Padded format (e.g., ACMY01)
  COL_FORMAT_SIMPLE = "%s%d" # Simple format (e.g., y1)
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
  FED_SVENSSON = "https://www.federalreserve.gov/data/yield-curve-tables/feds200628.csv"
)
