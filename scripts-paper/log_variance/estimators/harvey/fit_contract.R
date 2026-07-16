# Pure helpers for the Harvey estimator object: the accepted-fit predicate, the
# analytic implicit Jacobian, and the metadata-stamped cross-estimator start
# bundle. Split from the constructor module for the repository line cap.
# Definitions only; sourced by estimator.R after the math/recession/solver
# modules. The canonical spec_id serializer is the shared logvar_spec_id.

# A fit is accepted when it is a converged "ok" solution (NULL/failed rejected).
logvar_harvey_accepted <- function(fit) {
  !is.null(fit) && identical(fit$fit_status, "ok") && isTRUE(fit$converged)
}
# Analytic implicit Jacobian D_b theta_hat_H(b), returned only for an accepted
# "ok" fit with strictly positive finite variances and well-conditioned
# observed information; else NULL so the engine's derivative-free path takes
# over. Explicit Cholesky of X' diag(r) X (= 2 * observed information), never an
# inverse; the RHS row-scales -2 e/mu (zero-safe on the log scale) down W2.
logvar_harvey_jacobian <- function(fit, b, w1, w2, x_mat) {
  if (!isTRUE(fit$converged) || !identical(fit$fit_status, "ok")) {
    return(NULL)
  }
  theta <- fit$warm_start
  e <- drop(w1 - w2 %*% b)
  eta <- drop(x_mat %*% theta)
  mu <- exp(eta)
  if (!all(is.finite(mu)) || any(mu <= 0)) {
    return(NULL)
  }
  a_mat <- 2 * logvar_harvey_info(theta, e^2, x_mat)
  if (!all(is.finite(a_mat)) || rcond(a_mat) < 1e-10) {
    return(NULL)
  }
  e_over_mu <- logvar_harvey_e_over_mu(e, eta)
  rhs <- crossprod(x_mat, (-2 * e_over_mu) * w2)
  r_chol <- tryCatch(chol(a_mat), error = function(cond) NULL)
  if (is.null(r_chol)) {
    return(NULL)
  }
  out <- backsolve(r_chol, forwardsolve(t(r_chol), rhs))
  rownames(out) <- colnames(x_mat)
  out
}
# The metadata-stamped cross-estimator start object: NULL unless fit is an
# accepted Harvey point fit, else a typed bundle with original-scale
# coefficients (no scaling) and sample_id/spec_id copied from metadata.
logvar_harvey_start_bundle <- function(fit, metadata) {
  if (!(identical(fit$fit_status, "ok") && isTRUE(fit$converged))) {
    return(NULL)
  }
  if (is.null(metadata$sample_id) || is.null(metadata$spec_id)) {
    stop("logvar_harvey_start_bundle: metadata needs sample_id and spec_id")
  }
  list(
    coef_original = fit$coef, valid_for = "variance_start",
    sample_id = metadata$sample_id, spec_id = metadata$spec_id
  )
}
