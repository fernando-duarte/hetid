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
#'   \item{DEFAULT_STEP}{Maturity-index units per news period (1).
#'     The news operator steps one period; maturity arithmetic moves
#'     in multiples of the step (see
#'     \code{\link{effective_max_maturity}})}
#'   \item{MATURITY_UNITS_PER_YEAR}{Divisor converting a maturity
#'     index to years (1). Log bond prices scale annualized yields
#'     by maturity in years, so the n_hat weights are
#'     \code{i / MATURITY_UNITS_PER_YEAR}}
#'   \item{MACHINE_EPSILON}{Machine precision}
#'   \item{MATRIX_SYMMETRY_TOL}{Tolerance for matrix symmetry
#'     checks}
#'   \item{DEGENERACY_TOLERANCE}{Scale-free threshold below which the
#'     variance positivity diagnostic flags a maturity as degenerate}
#'   \item{PERCENT_TO_DECIMAL}{Percentage to decimal divisor}
#'   \item{MONTHS_PER_QUARTER}{Calendar months per quarter (3)}
#'   \item{USE_INCOMPLETE_QUARTERS}{Default policy for quarters missing
#'     their terminal month in quarterly conversion: TRUE keeps them
#'     (re-dated to quarter end), FALSE drops them}
#'   \item{ACM_DATE_FORMAT}{Date format in ACM files}
#'   \item{ISO_DATE_FORMAT}{Standard ISO date format}
#'   \item{YEAR_FORMAT}{Year extraction format}
#'   \item{MONTH_FORMAT}{Month extraction format}
#'   \item{CONSUMPTION_GROWTH_COL}{Consumption growth column name}
#'   \item{PC_PREFIX}{Prefix for principal component columns}
#'   \item{MATURITY_PREFIX}{Prefix for maturity label columns}
#'   \item{ACM_DATA_FILENAME}{Bundled/downloaded ACM data filename
#'     (gzipped CSV from the GitHub reproduction release)}
#'   \item{ACM_NYFED_FILENAME}{Cache filename for the opt-in NY Fed
#'     xls fallback source}
#'   \item{ACM_LEGACY_FILENAME}{Retired cache filename from the old
#'     xls-to-CSV pipeline; never resolved, only flagged for cleanup}
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

  # News-period geometry
  DEFAULT_STEP = 1L, # Maturity-index units per news period
  MATURITY_UNITS_PER_YEAR = 1L, # Maturity index units in one year

  # Numerical parameters
  MACHINE_EPSILON = .Machine$double.eps,
  MATRIX_SYMMETRY_TOL = 1e-10, # Matrix symmetry check tol
  PERCENT_TO_DECIMAL = 100, # Divisor for percentage conversion
  DEGENERACY_TOLERANCE = 1e-8, # Variance positivity diagnostic

  # Calendar
  MONTHS_PER_QUARTER = 3L,
  USE_INCOMPLETE_QUARTERS = TRUE, # Keep (re-dated) vs drop incomplete quarters

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
  ACM_DATA_FILENAME = "ACMTermPremium_reproduced_monthly_6m_120m.csv.gz",
  ACM_NYFED_FILENAME = "ACMTermPremium_nyfed.csv",
  ACM_LEGACY_FILENAME = "ACMTermPremium.csv",
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
#'   \item{ACM_GITHUB_CSV_GZ}{Latest-release URL for the monthly-maturity
#'     ACM reproduction (gzipped CSV)}
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
  ACM_GITHUB_CSV_GZ = paste0(
    "https://github.com/fernando-duarte/ACM_term_premium/releases/",
    "latest/download/ACMTermPremium_reproduced_monthly_6m_120m.csv.gz"
  ),
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
