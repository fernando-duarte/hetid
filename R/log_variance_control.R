#' Log-Variance Estimation Controls
#'
#' @description
#' Numerical controls for the PPML log-variance estimator and its
#' standard errors, plus the HAC lag default shared by every estimator.
#' Ported from the paper pipeline's \code{LOGVAR_PPML_CONTROL}
#' (\code{scripts-paper/log_variance/estimators/controls.R}) and
#' \code{hac_lags} (\code{scripts-paper/config/reporting.R}). The Harvey
#' estimator's controls live in \code{\link{LOG_VARIANCE_HARVEY_CONTROL}}.
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
#'   \item{SE_TYPES}{The PPML estimator's standard-error types
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

#' Harvey Log-Variance Estimation Controls
#'
#' @description
#' Numerical controls for the Harvey (1976) Gaussian
#' multiplicative-heteroskedasticity log-variance estimator and its
#' standard errors. Ported from the paper pipeline's
#' \code{LOGVAR_HARVEY_CONTROL}
#' (\code{scripts-paper/log_variance/estimators/controls.R}), minus the
#' recession-certificate and start-policy fields the package does not carry.
#' The HAC lag default is shared with PPML and lives in
#' \code{\link{LOG_VARIANCE_CONTROL}$HAC_LAGS}.
#'
#' @format List containing Harvey estimation controls:
#' \describe{
#'   \item{SCORE_TOLERANCE}{Scaled-score convergence tolerance (1e-8)}
#'   \item{RANK_TOLERANCE}{\code{qr} tolerance for the positive-response
#'     design rank diagnostic (1e-8)}
#'   \item{RCOND_TOLERANCE}{Reciprocal-condition floor on the
#'     diagonally-normalized observed information at acceptance and in the
#'     standard errors (1e-10)}
#'   \item{NEWTON_RCOND_TOLERANCE}{Reciprocal-condition floor below which
#'     the observed-Newton direction is abandoned for the Fisher-scoring
#'     direction (1e-12)}
#'   \item{LINE_SEARCH_HALVINGS}{Maximum step halvings per line search
#'     (30L)}
#'   \item{Q_NOISE_MULTIPLIER}{Multiplier on the criterion's summation
#'     rounding error defining a criterion tie in the line search (4)}
#'   \item{SCORE_PROGRESS_MULTIPLIER}{Multiplier on machine epsilon setting
#'     the strict scaled-score improvement a tie must show (10)}
#'   \item{MAXIT}{Maximum scoring iterations (1000L)}
#'   \item{REL_CHANGE_TOLERANCE}{Relative criterion or parameter change
#'     required alongside a passed score to declare convergence (1e-10)}
#'   \item{SE_TYPES}{The Harvey estimator's standard-error types
#'     (\code{"expected"}, \code{"observed"}, \code{"opg"}, \code{"robust"},
#'     \code{"hac"})}
#' }
#'
#' @return A named list of Harvey estimation controls (the elements
#'   described in \strong{Format}). Access individual controls with
#'   \code{$}.
#' @examples
#' LOG_VARIANCE_HARVEY_CONTROL$MAXIT
#' LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES
#' @export
LOG_VARIANCE_HARVEY_CONTROL <- list(
  SCORE_TOLERANCE = 1e-8,
  RANK_TOLERANCE = 1e-8,
  RCOND_TOLERANCE = 1e-10,
  NEWTON_RCOND_TOLERANCE = 1e-12,
  LINE_SEARCH_HALVINGS = 30L,
  Q_NOISE_MULTIPLIER = 4,
  SCORE_PROGRESS_MULTIPLIER = 10,
  MAXIT = 1000L,
  REL_CHANGE_TOLERANCE = 1e-10,
  SE_TYPES = c("expected", "observed", "opg", "robust", "hac")
)
