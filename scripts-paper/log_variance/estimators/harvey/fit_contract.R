# Pure helpers for the Harvey estimator object: the accepted-fit predicate, the
# analytic implicit Jacobian, and the metadata-stamped cross-estimator start
# bundle. Split from the constructor module for the repository line cap.
# Definitions only; sourced by estimator.R after the math/recession/solver
# modules. The canonical spec_id serializer is the shared logvar_spec_id.

# A fit is accepted when it is a converged "ok" solution (NULL/failed rejected).
logvar_harvey_accepted <- function(fit) {
  !is.null(fit) && identical(fit$fit_status, "ok") && isTRUE(fit$converged)
}

logvar_harvey_validate_policy <- function(control) {
  fit_allowed <- c("warm", "ppml_at_b", "standalone")
  start_allowed <- c("ppml_point", "logols_shifted", "intercept_only")
  stopifnot(
    is.character(control$fit_stage_policy),
    length(control$fit_stage_policy) > 0L,
    !anyDuplicated(control$fit_stage_policy),
    all(control$fit_stage_policy %in% fit_allowed),
    is.character(control$standalone_start_policy),
    !anyDuplicated(control$standalone_start_policy),
    all(control$standalone_start_policy %in% start_allowed),
    identical(control$scaling_policy, "none")
  )
  invisible(control)
}

logvar_harvey_start_plan <- function(ladder, control) {
  logvar_harvey_validate_policy(control)
  policy <- control$standalone_start_policy
  available <- policy[policy != "intercept_only" & policy %in% names(ladder)]
  starts <- unname(ladder[available])
  list(
    start = if (length(starts)) starts[[1L]] else NULL,
    fallback_starts = if (length(starts) > 1L) starts[-1L] else list(),
    auto_intercept = "intercept_only" %in% policy
  )
}

logvar_harvey_start_identity <- function(
  ppml_bundle,
  ppml_bundle_source_id,
  ppml_start_at_b,
  ppml_start_at_b_source_id,
  logols_coef
) {
  source_digest <- function(value, source_id, label) {
    if (is.null(value)) {
      return("null")
    }
    if (!is.character(source_id) || length(source_id) != 1L ||
      is.na(source_id) || !nzchar(source_id)) {
      stop(sprintf("%s requires one nonempty source ID", label))
    }
    paper_sha256_string(source_id)
  }
  if (!is.null(ppml_start_at_b) && !is.function(ppml_start_at_b)) {
    stop("ppml_start_at_b must be NULL or a function")
  }
  list(
    ppml_bundle = if (is.null(ppml_bundle)) {
      "null"
    } else {
      paper_sha256_object(ppml_bundle)
    },
    ppml_bundle_source = source_digest(
      ppml_bundle,
      ppml_bundle_source_id,
      "ppml_bundle"
    ),
    logols_start = if (is.null(logols_coef)) {
      "null"
    } else {
      paper_sha256_object(logols_coef)
    },
    ppml_at_b_source = source_digest(
      ppml_start_at_b,
      ppml_start_at_b_source_id,
      "ppml_start_at_b"
    )
  )
}

# Analytic implicit Jacobian D_b theta_hat_H(b), returned only for an accepted
# "ok" fit with strictly positive finite variances and well-conditioned
# observed information; else NULL so the engine's derivative-free path takes
# over. Explicit Cholesky of X' diag(r) X (= 2 * observed information), never an
# inverse; the RHS row-scales -2 e/mu (zero-safe on the log scale) down W2.
logvar_harvey_jacobian <- function(
  fit, b, w1, w2, x_mat,
  control = LOGVAR_HARVEY_CONTROL
) {
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
  if (!all(is.finite(a_mat)) ||
    rcond(a_mat) < control$jacobian_rcond_tol) {
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
