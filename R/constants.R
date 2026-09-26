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
#'   \item{MIN_MATURITY}{Minimum maturity index in months (1)}
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
#'     (1:120 months) available from the GitHub source}
#'   \item{DEGENERACY_TOLERANCE}{Scale-free threshold below which the
#'     variance positivity diagnostic flags a maturity as degenerate}
#'   \item{TAU0_POINT_TOLERANCE}{Rank and residual tolerance for the
#'     tau = 0 stacked linear solve; the alternative outcome is no point (NULL)}
#'   \item{QUADRATIC_EVIDENCE_MAXIT}{Default multivariate evidence-search budget (500)}
#'   \item{QUADRATIC_SEARCH_RTOL}{Candidate geometry-search tolerance (1e-10)}
#'   \item{QUADRATIC_OUTER_RTOL}{Candidate outer-bound search tolerance (1e-12)}
#'   \item{QUADRATIC_SIGN_FACTOR}{Rounding-margin multiplier for sign checks (64)}
#'   \item{QUADRATIC_OUTER_FACTOR}{Rounding-margin multiplier for outer bounds (16)}
#'   \item{QUADRATIC_WEIGHT_FLOOR}{Interior weight-search initialization floor (1e-9)}
#'   \item{PERCENT_TO_DECIMAL}{Percentage to decimal divisor}
#'   \item{MONTHS_PER_QUARTER}{Calendar months per quarter (3)}
#'   \item{MONTHS_PER_YEAR}{Calendar months per year (12), used for
#'     period-end month arithmetic}
#'   \item{USE_INCOMPLETE_QUARTERS}{Default policy for quarters missing
#'     their terminal month in quarterly conversion: TRUE keeps them
#'     (re-dated to quarter end), FALSE drops them}
#'   \item{ACM_DATE_FORMAT}{Date format in ACM files}
#'   \item{ISO_DATE_FORMAT}{Standard ISO date format}
#'   \item{ISO_TIMESTAMP_FORMAT}{UTC timestamp format for provenance}
#'   \item{YEAR_FORMAT}{Year extraction format}
#'   \item{MONTH_FORMAT}{Month extraction format}
#'   \item{CONSUMPTION_GROWTH_COL}{Consumption growth column name}
#'   \item{PC_PREFIX}{Prefix for principal component columns}
#'   \item{MATURITY_PREFIX}{Prefix for maturity label columns}
#'   \item{ACM_DATA_FILENAME}{Bundled/downloaded monthly ACM data filename
#'     (gzipped CSV from the GitHub replication release)}
#'   \item{ACM_DAILY_DATA_FILENAME}{Downloaded daily ACM data filename
#'     (gzipped CSV, GitHub release only, never bundled)}
#'   \item{ACM_NYFED_FILENAME}{Cache filename for the opt-in NY Fed
#'     xls fallback source}
#'   \item{BUNDLED_VARIABLES_DATASET}{Bundled dataset name}
#'   \item{COL_FORMAT_PADDED}{Padded raw column name format for
#'     whole-year maturities (e.g. ACMY01)}
#'   \item{COL_FORMAT_MONTHLY}{Raw column name format for sub-annual
#'     month maturities (e.g. ACMY003M)}
#'   \item{COL_FORMAT_SIMPLE}{Package column name format, maturity in
#'     months (e.g. y12)}
#'   \item{INSUFFICIENT_NEWS_MSG}{Shared error message for the
#'     news-period data-sufficiency guard in the variance-bound kernels}
#'   \item{BOUND_INDEX_TRIM_MSG}{Shared step-multiple reason string for the
#'     bound-index-set kernels (c_hat/k2_hat)}
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
#' @return A named list of package constants (the elements described in
#'   \strong{Format}). Access individual constants with \code{$}.
#' @examples
#' HETID_CONSTANTS$DEFAULT_N_PCS
#' HETID_CONSTANTS$DEFAULT_ACM_MATURITIES
#' @export
HETID_CONSTANTS <- list(
  # Principal component defaults
  DEFAULT_N_PCS = 4L, # Default from Adrian, Crump, Moench (2013)
  MAX_N_PCS = 6L, # Maximum for stability

  # Data constraints (maturity indices are months)
  MIN_MATURITY = 1L,
  MAX_MATURITY = 120L,

  # News-period geometry
  DEFAULT_STEP = 12L,
  MATURITY_UNITS_PER_YEAR = 12L,

  # Shared guard message for the news-period kernels (c_hat/k_hat/k2_hat/gap)
  INSUFFICIENT_NEWS_MSG = "Not enough observations. Need T > i/step news periods",
  BOUND_INDEX_TRIM_MSG = "the bound index set trims whole news periods",

  # Maturity grids (months)
  DEFAULT_ACM_MATURITIES = seq(12L, 120L, by = 12L),
  ALL_ACM_MATURITIES = 1L:120L,

  # Numerical parameters
  PERCENT_TO_DECIMAL = 100,
  DEGENERACY_TOLERANCE = 1e-8,
  TAU0_POINT_TOLERANCE = 1e-8,
  QUADRATIC_EVIDENCE_MAXIT = 500L,
  QUADRATIC_SEARCH_RTOL = 1e-10,
  QUADRATIC_OUTER_RTOL = 1e-12,
  QUADRATIC_SIGN_FACTOR = 64,
  QUADRATIC_OUTER_FACTOR = 16,
  QUADRATIC_WEIGHT_FLOOR = 1e-9,

  # Calendar
  MONTHS_PER_QUARTER = 3L,
  MONTHS_PER_YEAR = 12L,
  USE_INCOMPLETE_QUARTERS = TRUE,

  # Date formats
  ACM_DATE_FORMAT = "%d-%b-%Y",
  ISO_DATE_FORMAT = "%Y-%m-%d",
  ISO_TIMESTAMP_FORMAT = "%Y-%m-%dT%H:%M:%SZ",
  YEAR_FORMAT = "%Y",
  MONTH_FORMAT = "%m",

  # Column names
  CONSUMPTION_GROWTH_COL = "gr1.pcecc96",
  PC_PREFIX = "pc",
  MATURITY_PREFIX = "maturity_",

  # Data identity
  ACM_DATA_FILENAME = "ACMTermPremium_replicated_monthly_1m_120m.csv.gz",
  ACM_DAILY_DATA_FILENAME = "ACMTermPremium_replicated_daily_1m_120m.csv.gz",
  ACM_NYFED_FILENAME = "ACMTermPremium_nyfed.csv",
  BUNDLED_VARIABLES_DATASET = "variables",

  # Column format patterns
  COL_FORMAT_PADDED = "%s%02d",
  COL_FORMAT_MONTHLY = "%s%03dM",
  COL_FORMAT_SIMPLE = "%s%d"
)
