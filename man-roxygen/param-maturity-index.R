#' @param i Integer specifying the maturity index (column) to analyze.
#'   Must be between 1 and the package maximum (10 for the ACM dataset;
#'   functions that require data at maturity \code{i + 1} accept at most 9).
#'   The input data must contain the columns for the maturities the function
#'   uses (e.g. \code{y5}/\code{tp5} plus adjacent maturities for \code{i = 5}).
#'   Corresponds to maturity in years for the ACM dataset (i=1 is 1-year,
#'   i=2 is 2-year, ..., i=10 is 10-year).
