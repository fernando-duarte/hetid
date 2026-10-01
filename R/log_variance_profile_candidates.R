#' Candidate Points Inside the Identified Set
#'
#' All sampled rows, including witnesses, are re-checked with the relative
#' feasibility tolerance. The set can be non-convex, so a point between two
#' of its members need not belong to it.
#' Witness rows containing missing values are omitted before sampling.
#' Nonfinite feasibility arithmetic raises a structured error.
#'
#' @param box A validated \code{hetid_theta_box} object.
#' @param n_points A positive integer giving the number of equally spaced steps
#'   from the center toward each witness, including the witness itself.
#' @return A numeric matrix with one row per distinct feasible candidate and one
#'   column per \code{theta} component. Returns \code{NULL} when a box bound
#'   is nonfinite, no complete witness remains, or no candidate passes the check.
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
#' Creates a coefficient-bound frame when no fit is available.
#'
#' @param coef_labels A character vector of coefficient labels.
#' @param n_attempted,n_failed Integer counts of attempted and failed fits,
#'   respectively.
#' @param estimator A character string identifying the estimator.
#' @return A data frame with one row per coefficient label and columns
#'   \code{term}, \code{lower}, and \code{upper}. Both bounds are \code{NA_real_}.
#'   Attributes \code{n_attempted}, \code{n_failed}, and \code{estimator} retain
#'   the supplied counts and estimator identifier.
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
