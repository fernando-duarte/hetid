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
#'   \item{MIN_MATURITY}{Minimum maturity index in months (3)}
#'   \item{MAX_MATURITY}{Maximum maturity index in months (120)}
#'   \item{DEFAULT_STEP}{Maturity-index units (months) per news period
#'     (12: an annual news clock). The news operator steps one period;
#'     maturity arithmetic moves in multiples of the step (see
#'     \code{\link{effective_max_maturity}})}
#'   \item{MATURITY_UNITS_PER_YEAR}{Divisor converting a maturity
#'     index to years (12: indices are months). Log bond prices scale
#'     annualized yields by maturity in years, so the n_hat weights are
#'     \code{i / MATURITY_UNITS_PER_YEAR}}
#'   \item{DEFAULT_ACM_MATURITIES}{Annual maturity nodes in months
#'     (12, 24, ..., 120); the default for
#'     \code{\link{extract_acm_data}}}
#'   \item{ALL_ACM_MATURITIES}{The full monthly maturity grid
#'     (3:120 months) available from the GitHub source}
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
#'     (gzipped CSV from the GitHub replication release)}
#'   \item{ACM_NYFED_FILENAME}{Cache filename for the opt-in NY Fed
#'     xls fallback source}
#'   \item{ACM_LEGACY_FILENAMES}{Retired cache filenames (the old
#'     xls-to-CSV pipeline and superseded release assets); never
#'     resolved, only flagged for cleanup}
#'   \item{BUNDLED_VARIABLES_DATASET}{Bundled dataset name}
#'   \item{COL_FORMAT_PADDED}{Padded raw column name format for
#'     whole-year maturities (e.g. ACMY01)}
#'   \item{COL_FORMAT_MONTHLY}{Raw column name format for sub-annual
#'     month maturities (e.g. ACMY003M)}
#'   \item{COL_FORMAT_SIMPLE}{Package column name format, maturity in
#'     months (e.g. y12)}
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

  # Data constraints (maturity indices are months)
  MIN_MATURITY = 3L, # Minimum available maturity (months)
  MAX_MATURITY = 120L, # Maximum available maturity (months)

  # News-period geometry
  DEFAULT_STEP = 12L, # Maturity-index units (months) per news period
  MATURITY_UNITS_PER_YEAR = 12L, # Maturity index units in one year

  # Maturity grids (months)
  DEFAULT_ACM_MATURITIES = seq(12L, 120L, by = 12L), # Annual nodes
  ALL_ACM_MATURITIES = 3L:120L, # Full monthly grid

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
  ACM_DATA_FILENAME = "ACMTermPremium_replicated_monthly_3m_120m.csv.gz",
  ACM_NYFED_FILENAME = "ACMTermPremium_nyfed.csv",
  ACM_LEGACY_FILENAMES = c(
    "ACMTermPremium.csv",
    "ACMTermPremium_reproduced_monthly_6m_120m.csv.gz"
  ),
  BUNDLED_VARIABLES_DATASET = "variables",

  # Column format patterns
  COL_FORMAT_PADDED = "%s%02d", # e.g., ACMY01 (whole-year raw names)
  COL_FORMAT_MONTHLY = "%s%03dM", # e.g., ACMY003M (sub-annual raw names)
  COL_FORMAT_SIMPLE = "%s%d" # e.g., y12 (package names, months)
)

#' Data Source URLs
#'
#' URLs for external data sources.
#' These URLs are documented for data access.
#'
#' @format List containing versioned data source URLs:
#' \describe{
#'   \item{ACM_GITHUB_CSV_GZ}{Latest-release URL for the monthly-maturity
#'     ACM replication (gzipped CSV)}
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
    "latest/download/ACMTermPremium_replicated_monthly_3m_120m.csv.gz"
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
