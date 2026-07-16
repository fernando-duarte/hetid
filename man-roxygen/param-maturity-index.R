#' @param i Integer specifying the maturity index (column) to analyze,
#'   in \strong{months}, on the ACM monthly grid (1 to 120). Each
#'   function documents its own accepted range: those requiring data
#'   at maturity \code{i + step} accept at most
#'   \code{MAX_MATURITY - step}, and the expected-SDF pair also
#'   accepts \code{i = 0} (the exact horizon-zero boundary). The
#'   input data must
#'   contain the columns for the maturities the function uses
#'   (e.g. \code{y60}/\code{tp60} plus step-adjacent maturities for
#'   \code{i = 60}). Whole years are multiples of 12 (i = 12 is the
#'   1-year bond, i = 60 the 5-year, i = 120 the 10-year).
