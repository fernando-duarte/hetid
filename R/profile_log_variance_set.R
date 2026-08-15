#' Profile the Log-Variance Coefficients Over an Identified Set
#'
#' Fits the log-variance equation at points of the mean-equation
#' identified set and reports, for each volatility coefficient, the range
#' the fits span. This is the slack-\eqn{\tau} counterpart of
#' \code{\link{fit_log_variance_at_b}}, which fits at a single \eqn{b}.
#'
#' @details
#' Candidate points are the box's attaining witnesses together with steps
#' from the center toward each of them, and every candidate is re-checked
#' against the constraints before it is fitted, so no fit is ever run
#' outside the set.
#'
#' Fits that fail are skipped rather than fatal, and the counts are
#' reported. Skipping can only narrow the reported range, never widen it:
#' the range is over points that were fitted successfully, and every one
#' of those lies in the set. Warm starts are carried from the last
#' \emph{successful} fit; because the quasi-Poisson log link is canonical
#' the fit is unique, so a start affects whether a fit converges but never
#' which answer it converges to.
#'
#' @section Interpretation:
#' The range is \strong{attained over the sampled points}, not the profile
#' over the whole set. It is an inner approximation on both counts: the
#' box itself is one, and the sample is finite. Widen it by raising
#' \code{n_points}, or the box's \code{n_grid}.
#'
#' @param box A \code{hetid_theta_box} from
#'   \code{\link{compute_identified_set_box}}
#' @param x_var Volatility-equation design, without an intercept column.
#'   This is a different design from the mean equation's \code{x}
#' @param estimator Estimator id passed through to
#'   \code{\link{fit_log_variance_at_b}}, which owns the valid set
#' @param n_points Steps from the center toward each witness; defaults to
#'   \code{IDENTIFIED_SET_CONTROL$N_POINTS}
#' @return A data frame with \code{term}, \code{lower} and \code{upper},
#'   one row per volatility coefficient, all \code{NA} when no candidate
#'   could be fitted or the box has an infinite side. Attributes
#'   \code{n_attempted}, \code{n_failed} and \code{estimator} record the
#'   sampling.
#' @seealso \code{\link{compute_identified_set_box}} for the box,
#'   \code{\link{fit_log_variance_at_b}} for the single-\eqn{b} fit
#' @export
#'
#' @examples
#' set.seed(42)
#' n_obs <- 200
#' x <- cbind(x1 = rnorm(n_obs), x2 = rnorm(n_obs))
#' z <- rnorm(n_obs)
#' e2 <- sqrt(exp(0.5 + 0.9 * z)) * matrix(rnorm(n_obs * 2), n_obs, 2)
#' y2 <- x %*% matrix(c(1, 0.5, -0.3, 0.7), 2, 2) + e2
#' colnames(y2) <- c("news1", "news2")
#' y1 <- drop(0.3 + x %*% c(0.2, -0.1) + y2 %*% c(0.8, -0.5) + rnorm(n_obs))
#' x_var <- cbind(v1 = rnorm(n_obs), v2 = rnorm(n_obs))
#'
#' fit <- compute_tau0_system(y1, y2, x, z)
#' box <- compute_identified_set_box(fit, tau = 0.05, n_grid = 11L)
#' profile_log_variance_set(box, x_var)
profile_log_variance_set <- function(box, x_var, estimator = "ppml",
                                     n_points =
                                       IDENTIFIED_SET_CONTROL$N_POINTS) {
  assert_hetid_theta_box(box)
  assert_scalar_integer_in_range(n_points, "n_points", 1, .Machine$integer.max)
  coef_labels <- colnames(log_variance_design(x_var))

  candidates <- profile_set_candidates(box, n_points)
  if (is.null(candidates)) {
    return(empty_log_variance_profile(coef_labels, 0L, 0L, estimator))
  }
  fits <- fit_over_candidates(candidates, box, x_var, estimator)
  if (is.null(fits$coefs)) {
    return(empty_log_variance_profile(
      coef_labels, nrow(candidates), fits$n_failed, estimator
    ))
  }
  out <- data.frame(
    term = colnames(fits$coefs),
    lower = apply(fits$coefs, 2, min),
    upper = apply(fits$coefs, 2, max),
    row.names = NULL
  )
  attr(out, "n_attempted") <- nrow(candidates)
  attr(out, "n_failed") <- fits$n_failed
  attr(out, "estimator") <- estimator
  out
}

#' Candidate Points Inside the Identified Set
#'
#' The witnesses are feasible by construction and the interpolations are
#' re-checked, because the set is non-convex and a point between two of
#' its members need not belong to it.
#'
#' @param box A \code{hetid_theta_box}
#' @param n_points Steps from the center toward each witness
#' @return Numeric matrix of distinct feasible candidates, or \code{NULL}
#'   when the box has an infinite side or nothing survives the check
#' @noRd
profile_set_candidates <- function(box, n_points) {
  if (any(!is.finite(box$bounds$lower)) || any(!is.finite(box$bounds$upper))) {
    return(NULL)
  }
  witnesses <- rbind(box$arg_lower, box$arg_upper)
  witnesses <- witnesses[stats::complete.cases(witnesses), , drop = FALSE]
  if (nrow(witnesses) == 0L) {
    return(NULL)
  }
  center <- colMeans(witnesses)
  steps <- seq_len(n_points) / n_points
  sampled <- rbind(
    center,
    do.call(rbind, lapply(steps, function(s) {
      sweep(witnesses * s, 2, center * (1 - s), "+")
    }))
  )
  checker <- make_system_checker(box$quadratic)
  keep <- apply(sampled, 1, function(b) max(checker(b))) <=
    IDENTIFIED_SET_CONTROL$FEAS_TOL
  sampled <- unique(sampled[keep, , drop = FALSE])
  if (nrow(sampled) == 0L) NULL else sampled
}

#' Fit the Log-Variance Equation at Every Candidate
#'
#' @param candidates Numeric matrix of feasible points
#' @param box A \code{hetid_theta_box}
#' @param x_var Volatility-equation design
#' @param estimator Estimator id
#' @return List with \code{coefs} (matrix, or \code{NULL}) and
#'   \code{n_failed}
#' @noRd
fit_over_candidates <- function(candidates, box, x_var, estimator) {
  rows <- vector("list", nrow(candidates))
  warm <- NULL
  n_failed <- 0L
  for (i in seq_len(nrow(candidates))) {
    fit <- fit_log_variance_at_b(
      candidates[i, ], box$w1, box$w2, x_var,
      estimator = estimator, start = warm
    )
    if (log_variance_fit_ok(fit)) {
      rows[[i]] <- fit$coef
      warm <- fit$warm_start
    } else {
      n_failed <- n_failed + 1L
    }
  }
  rows <- rows[!vapply(rows, is.null, logical(1))]
  list(
    coefs = if (length(rows) == 0L) NULL else do.call(rbind, rows),
    n_failed = n_failed
  )
}

#' All-Missing Profile Frame
#'
#' @param coef_labels Coefficient labels
#' @param n_attempted,n_failed Sampling counts
#' @param estimator Estimator id
#' @return A data frame of NA bounds carrying the sampling attributes
#' @noRd
empty_log_variance_profile <- function(coef_labels, n_attempted, n_failed,
                                       estimator) {
  out <- data.frame(
    term = coef_labels,
    lower = NA_real_,
    upper = NA_real_,
    row.names = NULL
  )
  attr(out, "n_attempted") <- n_attempted
  attr(out, "n_failed") <- n_failed
  attr(out, "estimator") <- estimator
  out
}
