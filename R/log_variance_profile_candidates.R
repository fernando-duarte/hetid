#' Candidate Points Inside the Identified Set
#'
#' All sampled rows, including witnesses, are re-checked with the relative
#' feasibility tolerance. The set can be non-convex, so a point between two
#' of its members need not belong to it.
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
  checker <- make_relative_feasibility_checker(box$quadratic)
  keep <- apply(sampled, 1, checker)
  sampled <- unique(sampled[keep, , drop = FALSE])
  if (nrow(sampled) == 0L) NULL else sampled
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

log_variance_profile_bounds <- function(fits, n_attempted, labels, estimator) {
  if (is.null(fits$coefs)) {
    return(empty_log_variance_profile(labels, n_attempted, fits$n_failed, estimator))
  }
  out <- data.frame(
    term = colnames(fits$coefs), lower = apply(fits$coefs, 2, min),
    upper = apply(fits$coefs, 2, max), row.names = NULL
  )
  attr(out, "n_attempted") <- n_attempted
  attr(out, "n_failed") <- fits$n_failed
  attr(out, "estimator") <- estimator
  out
}
