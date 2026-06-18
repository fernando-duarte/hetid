#' @param i Integer specifying the maturity index (column) to analyze,
#'   in \strong{months}. Must be between 1 and 120 (the ACM monthly
#'   grid); functions that require data at maturity \code{i + step}
#'   accept at most \code{MAX_MATURITY - step}, i.e. 108 with the
#'   default annual step. The input data must contain the columns for
#'   the maturities the function uses (e.g. \code{y60}/\code{tp60}
#'   plus step-adjacent maturities for \code{i = 60}). Whole years are
#'   multiples of 12 (i = 12 is the 1-year bond, i = 60 the 5-year,
#'   i = 120 the 10-year).
