# Pure machinery for the log-variance equation
#   log eps_{t+1}^2 = theta_0 + PC_{R,t}' theta_R + xi_{t+1}
# mapped over the mean equation's set-identified news coefficients b_N: given
# b_N, eps_hat(b_N) = w1 - W2 b_N and the two-step estimator is the fixed
# projection theta_hat(b_N) = P log(eps_hat(b_N)^2), P = (R'R)^{-1} R',
# R = (1, PC_R). The identified set of each coefficient is the range of
# theta_hat over the joint b_N set: a feasible-grid scan plus an SLSQP
# polish, guarded by a residual-zero census -- log(eps^2) is singular where
# a residual crosses zero, and the hyperplane w2_t' b = w1_t meets the set
# iff w1_t lies inside the range of the linear functional w2_t' b over it,
# which crossing_census.R decides with verified outer bounds for no-crossing
# verdicts and checked member points for crossings (the scan's sign tracker
# is a second sound detector). Definitions only;
# sourced by the log-OLS orchestrator after the profile-bound internals and by
# tests/engine/test_residual_map.R.

paper_source_once(paper_path(
  "log_variance", "estimators", "controls.R"
))
paper_source_once(paper_path(
  "log_variance", "core", "crossing_census.R"
))
paper_source_once(paper_path(
  "log_variance", "core", "projection_scan.R"
))

logvar_design_matrix <- function(pcr, expected_pc_cols = NULL) {
  pcr <- as.matrix(pcr)
  if (is.null(colnames(pcr))) {
    stopifnot(ncol(pcr) <= length(
      PAPER_ANALYSIS_CONTRACT$model$return_pc_cols
    ))
    colnames(pcr) <-
      PAPER_ANALYSIS_CONTRACT$model$return_pc_cols[
        seq_len(ncol(pcr))
      ]
  }
  stopifnot(
    is.numeric(pcr),
    !anyNA(colnames(pcr)),
    all(nzchar(colnames(pcr))),
    !anyDuplicated(colnames(pcr))
  )
  if (!is.null(expected_pc_cols)) {
    stopifnot(identical(colnames(pcr), expected_pc_cols))
  }
  out <- cbind(rep(1, nrow(pcr)), pcr)
  colnames(out) <- c(
    PAPER_ANALYSIS_CONTRACT$model$intercept_col,
    colnames(pcr)
  )
  out
}

# projection rows P = (R'R)^{-1} R' of the log-variance regression, from the
# package's thin-QR operator; row j gives theta_hat_j(b) = P[j, ] %*%
# log((w1 - W2 b)^2)
logvar_projection <- function(pcr) {
  r_mat <- logvar_design_matrix(pcr)
  hetid::log_projection_matrix(r_mat[, -1L, drop = FALSE])
}

# theta_hat(b): OLS coefficients of log((w1 - W2 b)^2) on (1, PC_R)
logvar_theta_hat <- function(b, w1, w2, proj) {
  drop(proj %*% log(drop(w1 - w2 %*% b)^2))
}

# gradient of one coefficient theta_hat_j(b) = proj_row' log((w1 - W2 b)^2):
# d theta_hat_j / d b = -2 W2' (proj_row / eps_hat(b))
logvar_theta_grad <- function(b, w1, w2, proj_row) {
  -2 * drop(crossprod(w2, proj_row / drop(w1 - w2 %*% b)))
}

# full Jacobian of the map, J(b) = -2 P diag(1/eps_hat(b)) W2: dividing the
# n x K matrix w2 by the n-vector eps recycles down columns (row-wise), so
# row j equals logvar_theta_grad(b, w1, w2, proj[j, ])
logvar_theta_jacobian <- function(b, w1, w2, proj) {
  -2 * (proj %*% (w2 / drop(w1 - w2 %*% b)))
}

# axis-product grid over the per-coefficient bounding box, filtered to the
# points satisfying every quadratic constraint at a roundoff-scale normalized
# tolerance (a hard g <= 0 would shed exact-boundary lattice points of a thin
# set; the admission tolerance is stricter than the solver certificate)
logvar_feasible_grid <- function(qs, lower, upper, n_axis) {
  axes <- Map(function(lo, hi) seq(lo, hi, length.out = n_axis), lower, upper)
  b_grid <- as.matrix(expand.grid(axes, KEEP.OUT.ATTRS = FALSE))
  dimnames(b_grid) <- NULL
  omega <- .derive_constraint_scales(qs, .derive_theta_scale(qs))
  values <- quadratic_constraint_values(b_grid, qs, omega)
  feas <- apply(
    values <= PAPER_QUADRATIC_CONTROL$admission_tolerance,
    1L,
    all
  )
  b_grid[feas, , drop = FALSE]
}

# scan theta_hat over the feasible grid in chunks (the log-OLS benchmark
# arithmetic), through the estimator-neutral scanner in projection_scan.R;
# cross_grid is the both-signs crossing tracker, a sound crossing detector
# complementing the census
logvar_grid_scan <- function(b_feas, w1, w2, proj,
                             chunk = LOGVAR_SEARCH_CONTROL$scan_chunk_size) {
  logvar_projection_scan(
    b_feas,
    function(b) {
      list(
        coef = proj %*% log((w1 - w2 %*% t(b))^2),
        eligible = rep(TRUE, nrow(b))
      )
    },
    nrow(proj), chunk,
    residuals_at = function(b) w1 - w2 %*% t(b)
  )
}

# the endpoint polish (generalized objective seam plus the legacy
# logvar_polish_bound wrapper) lives in its own module so this file stays
# below the repository line cap; sourced here so existing callers see the
# same definitions
paper_source_once(paper_path("log_variance", "core", "endpoint_polish.R"))
