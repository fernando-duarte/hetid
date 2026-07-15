# The stacked log/PPML moment layer for the joint log-variance system (Plan 4,
# logvar-joint-gmm): the two variance-equation moment vectors on the aligned
# design x_mat = cbind(1, PC_R), their observation-level contributions and the
# frozen root-mean-square scales, the analytic moment Jacobians in the news
# coefficients b and in the variance parameters (a, beta), the singular-value
# rank diagnostic those Jacobians share, and the Stage A design rank
# precondition. Cores are side-effect-free -- the driver owns the joins, so
# every function takes already-aligned matrices with e = drop(w1 - w2 %*% b)
# the length-n residual and c(a, beta) the length-p intercept/slope stack. No
# clamping, no epsilon on log(e^2), no suppressed warnings: a zero or nonfinite
# scale and a rank-deficient design fail closed. Definitions only; sourced by
# test_logvar_joint_gmm.R and the Stage B/C driver.

# Stop with a classed moment-layer error (mirrors logvar_joint_decision_stop) so
# tests and callers dispatch on the exact reason string.
logvar_moment_stop <- function(reason, detail = "") {
  stop(structure(
    class = c(reason, "logvar_moment_error", "error", "condition"),
    list(
      message = if (nzchar(detail)) paste0(reason, ": ", detail) else reason,
      call = NULL
    )
  ))
}

# Log-variance moment X'{log(e^2) - X (a_L, beta)} / n at the residual e(b).
logvar_moment_log <- function(b, a_L, beta, w1, w2, x_mat) {
  e <- drop(w1 - w2 %*% b)
  resid <- log(e^2) - drop(x_mat %*% c(a_L, beta))
  drop(crossprod(x_mat, resid)) / length(e)
}

# PPML moment X'{e^2 - exp(X (a_P, beta))} / n on the same residual.
logvar_moment_ppml <- function(b, a_P, beta, w1, w2, x_mat) {
  e <- drop(w1 - w2 %*% b)
  resid <- e^2 - exp(drop(x_mat %*% c(a_P, beta)))
  drop(crossprod(x_mat, resid)) / length(e)
}

# Observation-level log contributions x_i {log(e_i^2) - x_i' (a_L, beta)}; their
# column means are logvar_moment_log.
logvar_moment_log_contrib <- function(b, a_L, beta, w1, w2, x_mat) {
  e <- drop(w1 - w2 %*% b)
  x_mat * (log(e^2) - drop(x_mat %*% c(a_L, beta)))
}

# Observation-level PPML contributions x_i {e_i^2 - exp(x_i' (a_P, beta))}, the
# natural sibling used for the downstream PPML scales.
logvar_moment_ppml_contrib <- function(b, a_P, beta, w1, w2, x_mat) {
  e <- drop(w1 - w2 %*% b)
  x_mat * (e^2 - exp(drop(x_mat %*% c(a_P, beta))))
}

# Frozen per-column moment scales: the root-mean-square of the observation
# contributions. A zero or nonfinite column is a degenerate scale, never a
# rescaling knob, so it fails closed.
logvar_moment_scales <- function(contribs) {
  scales <- sqrt(colMeans(contribs^2))
  if (any(!is.finite(scales)) || any(scales == 0)) {
    logvar_moment_stop("degenerate_moment_scale", "zero or nonfinite scale")
  }
  scales
}

# Log-block Jacobian in b: -2 X' diag(1/e) W2 / n (each column of w2 divided by
# the residual e). The 1/e weight inherits the benchmark crossing singularity.
logvar_jac_log_b <- function(b, w1, w2, x_mat) {
  e <- drop(w1 - w2 %*% b)
  -2 * crossprod(x_mat, w2 / e) / length(e)
}

# PPML-block Jacobian in b: -2 X' diag(e) W2 / n. The same sign as the log block
# but an e weight rather than 1/e -- the instructive log/PPML contrast.
logvar_jac_ppml_b <- function(b, w1, w2, x_mat) {
  e <- drop(w1 - w2 %*% b)
  -2 * crossprod(x_mat, e * w2) / length(e)
}

# Log-block Jacobian in the variance parameters: -X'X / n, constant in
# (a_L, beta) because the moment is linear there.
logvar_jac_log_theta <- function(x_mat) {
  -crossprod(x_mat) / nrow(x_mat)
}

# PPML-block Jacobian in the variance parameters: -X' diag(mu_P) X / n with the
# fitted mean mu_P = exp(X (a_P, beta)).
logvar_jac_ppml_theta <- function(b, a_P, beta, w1, w2, x_mat) {
  mu_p <- exp(drop(x_mat %*% c(a_P, beta)))
  -crossprod(x_mat, mu_p * x_mat) / nrow(x_mat)
}

# Shared singular-value diagnostic for the unprofiled and profiled Jacobians.
# cutoff = cutoff_factor * max(dim) * sigma_1; a retained singular value exceeds
# it, and a smallest retained value within ten cutoffs is flagged weak even when
# the formal rank is full.
logvar_jacobian_rank <- function(jac, cutoff_factor = 1e-8) {
  sv <- svd(jac, nu = 0, nv = 0)$d
  sigma_1 <- if (length(sv) > 0L) sv[1L] else 0
  cutoff <- cutoff_factor * max(dim(jac)) * sigma_1
  retained <- sv > cutoff
  rank <- sum(retained)
  min_retained <- if (rank > 0L) min(sv[retained]) else 0
  weak_jacobian <- rank > 0L && min_retained <= 10 * cutoff
  status <- if (rank == 0L) {
    "degenerate"
  } else if (rank < length(sv)) {
    "rank_deficient"
  } else if (weak_jacobian) {
    "weak"
  } else {
    "full_rank"
  }
  list(
    rank = rank, singular_values = sv, min_retained = min_retained,
    weak_jacobian = weak_jacobian, status = status
  )
}

# Stage A design precondition: normalize each nonzero column of x_mat by its
# Euclidean norm and require full numerical column rank. A nonfinite entry, a
# zero-norm column, or a deficient scaled rank stops before any projection or
# graph replication with input_rank_deficient.
logvar_joint_check_design <- function(x_mat, x_rank_tol = 1e-10) {
  if (any(!is.finite(x_mat))) {
    logvar_moment_stop("input_rank_deficient", "nonfinite design entry")
  }
  col_norms <- sqrt(colSums(x_mat^2))
  if (any(col_norms == 0)) {
    logvar_moment_stop("input_rank_deficient", "zero-norm design column")
  }
  x_scaled <- sweep(x_mat, 2L, col_norms, "/")
  sv <- svd(x_scaled, nu = 0, nv = 0)$d
  if (sum(sv > x_rank_tol * sv[1L]) != ncol(x_mat)) {
    logvar_moment_stop("input_rank_deficient", "scaled design rank deficient")
  }
  invisible(TRUE)
}
