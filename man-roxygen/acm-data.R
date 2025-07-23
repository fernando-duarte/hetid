#' @param yields Numeric matrix of yield curve data from Adrian, Crump, and Moench (2013).
#'   Rows represent time periods, columns represent maturities (1-10 years).
#'   Missing values should be coded as NA. All values in annualized percentage points.
#'
#' @param term_premia Numeric matrix of term premia data from Adrian, Crump, and Moench (2013).
#'   Rows represent time periods, columns represent maturities (1-10 years).
#'   Missing values should be coded as NA. All values in annualized percentage points.
#'   By construction: Term Premium = Yield - Risk-Neutral Yield.
#'
#' @section Data Source:
#' The ACM data is sourced from the Federal Reserve Bank of New York and is based on
#' the methodology described in Adrian, Crump, and Moench (2013). The data contains
#' model-implied yields, term premia, and risk-neutral yields for U.S. Treasury securities
#' with maturities from 1 to 10 years.
#'
#' @references
#' Adrian, T., Crump, R. K., and Moench, E. (2013). "Pricing the term structure
#' with linear regressions." Journal of Financial Economics, 110(1), 110-138.
