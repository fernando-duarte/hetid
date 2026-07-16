#' Transformed-Variable Naming Grammar Helpers
#'
#' Internal helpers that build column/coefficient names following the package's
#' transformed-variable naming grammar. The default unit is a quarter; a pure
#' lag of a level series \code{x} is \code{l.x} (lag 1, first digit dropped),
#' \code{l2.x}, \code{l3.x}, and so on.
#'
#' @keywords internal
#' @name naming
NULL

#' Grammar Names for Own-Lag Columns of a Series
#'
#' Builds the grammar lag-column names for a base series, dropping the first
#' digit: lag 1 is \code{l.<series>}, lag \code{h} (for \code{h >= 2}) is
#' \code{l<h>.<series>}.
#'
#' @param series Character scalar naming the base series.
#' @param n_lags Integer number of own-lags \eqn{H \ge 0}.
#'
#' @return Character vector of length \code{n_lags} with the grammar lag names.
#' @keywords internal
lag_grammar_names <- function(series, n_lags) {
  # vapply (not a vectorized paste0) so n_lags = 0 yields character(0): paste0
  # recycles a zero-length argument to "", which would wrongly return "l.<series>"
  vapply(seq_len(n_lags), function(h) {
    if (h == 1L) paste0("l.", series) else paste0("l", h, ".", series)
  }, character(1))
}
