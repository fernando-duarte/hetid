#' @param step Integer number of maturity-index units per news period
#'   (default \code{HETID_CONSTANTS$DEFAULT_STEP}). The news operator
#'   steps one period, so maturity arithmetic moves in multiples of
#'   \code{step}: \code{compute_n_hat()} pairs maturities \code{i} and
#'   \code{i + step}, the one-period bond is the \code{step}-maturity
#'   bond, and horizons must satisfy \code{i <= MAX_MATURITY - step}.
