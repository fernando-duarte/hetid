#' Log-Projection Controls
#'
#' @description
#' Tuning defaults and numerical tolerances for the log projections of
#' squared residuals computed by \code{\link{prepare_log_projection}} and
#' \code{\link{evaluate_log_projection}}.
#'
#' @format List containing:
#' \describe{
#'   \item{METHODS}{Supported methods (see \code{\link{evaluate_log_projection}})}
#'   \item{MULTIPLIER}{Primary tuning multiplier \eqn{m} (1)}
#'   \item{SENSITIVITY_MULTIPLIERS}{Multipliers for the tuning sensitivity
#'     check, halving and doubling the threshold (\code{c(0.5, 1, 2)})}
#'   \item{RANK_TOLERANCE}{\code{qr} tolerance for the volatility design and
#'     the news-residual matrix (1e-10)}
#'   \item{MEAN_ZERO_TOLERANCE}{Largest absolute mean of a mean-sample
#'     residual column, relative to its root mean square (1e-8)}
#'   \item{SCALE_TOLERANCE}{Ratio of the unrestricted Fuller scale lower
#'     bound to the common scale below which positivity is not certified
#'     (1e-12)}
#' }
#'
#' @return A named list of log-projection controls (the elements described
#'   in \strong{Format}). Access individual controls with \code{$}.
#' @examples
#' LOG_PROJECTION_CONTROL$METHODS
#' LOG_PROJECTION_CONTROL$SENSITIVITY_MULTIPLIERS
#' @export
LOG_PROJECTION_CONTROL <- list(
  METHODS = "log",
  MULTIPLIER = 1,
  SENSITIVITY_MULTIPLIERS = c(0.5, 1, 2),
  RANK_TOLERANCE = 1e-10,
  MEAN_ZERO_TOLERANCE = 1e-8,
  SCALE_TOLERANCE = 1e-12
)

LOG_PROJECTION_STATUS <- c(
  ok = "ok",
  domain_failure = "domain_failure",
  numerical_failure = "numerical_failure"
)
