#' Retain Joint Log-Variance Fits over Sampled Mean Parameters
#'
#' Uses the same feasible candidates as \code{\link{profile_log_variance_set}},
#' retaining each fitted coefficient vector beside its mean-parameter candidate.
#' Repeated responses share a prepared fixed-design fitter.
#'
#' @inheritParams profile_log_variance_set
#' @param estimator Estimator id passed to \code{\link{make_log_variance_fitter}}.
#' @param control Fitting controls passed to \code{\link{make_log_variance_fitter}}.
#' @param dates Optional unique, non-missing \code{Date} labels for the already
#'   aligned estimation rows. Supply period-end dates for a time series.
#' @details
#' Nonconverged fits are retained as failed records and omitted from the attained
#' ranges. Warm starts advance only after success. Invalid inputs and response
#' arithmetic errors still raise structured conditions. Infinite boxes are not
#' truncated: they return no fitted candidates, matching the existing sampler.
#' Candidate filtering uses a constraint-relative feasibility tolerance, shared
#' with linear-functional witness checks. Nonfinite check arithmetic raises a
#' structured error rather than admitting a point.
#'
#' These are sampled fitted-function ranges, not global extrema, confidence bands
#' or prediction intervals for future observations. The original box, fixed design,
#' dates and controls are retained so new predictions need no further fitting.
#' Align separate time series by date before constructing these inputs; this
#' function neither joins nor reorders observations.
#' @return A \code{hetid_log_variance_sample} list. \code{bounds} is the marginal
#'   coefficient frame, with attempted/failed counts as in the existing profiler.
#'   \code{candidates} and \code{coefficients} have matching candidate rows; failed
#'   coefficient rows are all NA. \code{fits} retains status and diagnostics per
#'   candidate without duplicating responses or designs. \code{reason} is
#'   \code{sampled}, \code{infinite_box}, \code{no_candidates}, or
#'   \code{all_fits_failed}. The remaining fields retain the fitting context.
#' @seealso \code{\link{predict.hetid_log_variance_sample}}
#' @export
#' @examples
#' set.seed(42)
#' n <- 100
#' z <- rnorm(n)
#' x <- cbind(x = rnorm(n))
#' y2 <- cbind(news = exp(z / 2) * rnorm(n))
#' y1 <- 0.3 + x[, 1] + 0.5 * y2[, 1] + rnorm(n)
#' fit <- compute_tau0_system(y1, y2, x, z)
#' box <- compute_identified_set_box(fit, 0.01, n_grid = 3)
#' sampled <- sample_log_variance_set(box, x)
#' predict(sampled, x[1:3, , drop = FALSE], type = "volatility")$bounds
sample_log_variance_set <- function(box, x_var, estimator = "ppml",
                                    n_points = IDENTIFIED_SET_CONTROL$N_POINTS,
                                    control = list(), dates = NULL) {
  validate_log_variance_sources(box)
  assert_scalar_integer_in_range(n_points, "n_points", 1, .Machine$integer.max)
  fitter <- make_log_variance_fitter(x_var, estimator, control)
  design <- log_variance_design(x_var)
  assert_dimension_ok(nrow(design) == nrow(box$w2), "x_var must have one row per observation")
  validate_log_variance_dates(dates, nrow(design))
  candidates <- profile_set_candidates(box, n_points)
  reason <- if (any(is.infinite(as.matrix(box$bounds[c("lower", "upper")])))) {
    "infinite_box"
  } else if (is.null(candidates)) {
    "no_candidates"
  } else {
    "sampled"
  }
  if (is.null(candidates)) candidates <- matrix(numeric(0), 0L, ncol(box$w2))
  dimnames(candidates) <- list(
    if (nrow(candidates)) paste0("candidate_", seq_len(nrow(candidates))) else NULL,
    colnames(box$w2)
  )
  assert_numeric_finite_values(candidates, "candidates")
  fit_candidate <- function(b, start) {
    eps <- drop(box$w1 - box$w2 %*% b)
    fit <- fitter(eps^2, start = start)
    fit$diagnostics$min_abs_eps <- min(abs(eps))
    fit
  }
  found <- fit_over_candidates(candidates, box, x_var, estimator,
    fitter = fit_candidate, retain = TRUE
  )
  bounds <- log_variance_profile_bounds(found, nrow(candidates), colnames(design), estimator)
  if (is.null(found$coefs) && nrow(candidates)) reason <- "all_fits_failed"
  out <- structure(
    c(list(
      bounds = bounds, candidates = candidates,
      box = box, x_design = design, estimator = estimator, n_points = n_points,
      control = log_variance_fit_control(estimator, control), dates = dates, reason = reason
    ), log_variance_sample_records(found$fits, candidates, colnames(design))),
    class = "hetid_log_variance_sample"
  )
  validate_hetid_log_variance_sample(out)
  out
}
