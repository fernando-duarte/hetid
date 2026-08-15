#' Log-Variance Estimation Controls
#'
#' @description
#' Numerical controls for log-variance (PPML) estimation and its
#' standard errors. Ported from the paper pipeline's
#' \code{LOGVAR_PPML_CONTROL} (\code{scripts-paper/log_variance/estimators/controls.R})
#' and \code{hac_lags} (\code{scripts-paper/config/reporting.R}).
#'
#' @format List containing log-variance estimation controls:
#' \describe{
#'   \item{GLM_EPSILON}{Convergence tolerance for \code{glm.fit} (1e-10)}
#'   \item{GLM_MAXIT}{Maximum \code{glm.fit} iterations (100L)}
#'   \item{SCORE_TOLERANCE}{Tolerance for the score-equation
#'     convergence check (1e-8)}
#'   \item{RANK_TOLERANCE}{Tolerance for detecting rank deficiency in
#'     the design matrix (1e-10)}
#'   \item{RCOND_TOLERANCE}{Tolerance for the reciprocal condition
#'     number below which a matrix is treated as ill-conditioned (1e-10)}
#'   \item{HAC_LAGS}{Newey-West lag truncation for HAC standard errors
#'     (4L). This is the paper's quarterly-data heuristic, not a
#'     statistical rule; users with other frequencies should pass their
#'     own \code{hac_lags}}
#'   \item{SE_TYPES}{Supported standard-error types
#'     (\code{"naive"}, \code{"hc0"}, \code{"hc1"}, \code{"hac"})}
#' }
#'
#' @return A named list of log-variance estimation controls (the elements
#'   described in \strong{Format}). Access individual controls with
#'   \code{$}.
#' @examples
#' LOG_VARIANCE_CONTROL$GLM_EPSILON
#' LOG_VARIANCE_CONTROL$SE_TYPES
#' @export
LOG_VARIANCE_CONTROL <- list(
  GLM_EPSILON = 1e-10,
  GLM_MAXIT = 100L,
  SCORE_TOLERANCE = 1e-8,
  RANK_TOLERANCE = 1e-10,
  RCOND_TOLERANCE = 1e-10,
  HAC_LAGS = 4L,
  SE_TYPES = c("naive", "hc0", "hc1", "hac")
)
