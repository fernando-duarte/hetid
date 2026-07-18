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
# which solve_linear_functional_bound answers up to its local-solver caveat
# (a closed-form box range soundly screens the unambiguous rows first, and
# the scan's sign tracker is a second sound detector). Definitions only;
# sourced by the log-OLS orchestrator after the profile-bound internals and by
# tests/engine/test_residual_map.R.

paper_source_once(paper_path(
  "log_variance", "estimators", "controls.R"
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

# projection rows P = (R'R)^{-1} R' of the log-variance regression; row j
# gives theta_hat_j(b) = P[j, ] %*% log((w1 - W2 b)^2)
logvar_projection <- function(pcr) {
  r_mat <- logvar_design_matrix(pcr)
  solve(crossprod(r_mat), t(r_mat))
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

# residual-zero census over the joint set: observation t's hyperplane
# w2_t' b = w1_t intersects the set iff w1_t lies inside the image of the
# linear functional w2_t' b over it -- an interval when the set is
# connected, so the [min, max] range test below is exact there and errs
# only toward flagging a crossing on a disconnected set. The closed-form
# range over the bounding box is a sound outer screen (outside it, no
# crossing is possible); each remaining ambiguous row gets functional
# bounds over the set itself, exact up to connectedness for found
# crossings and solver-certified for no-crossing verdicts. cross collects
# the certified crossings, unresolved the rows whose solves failed
# (callers must fail closed on those).
logvar_crossing_census <- function(qs, lower, upper, w1, w2) {
  w2_pos <- pmax(w2, 0)
  w2_neg <- pmin(w2, 0)
  box_min <- drop(w2_pos %*% lower + w2_neg %*% upper)
  box_max <- drop(w2_pos %*% upper + w2_neg %*% lower)
  cross <- integer(0)
  unresolved <- integer(0)
  for (t_row in which(w1 >= box_min & w1 <= box_max)) {
    # a zero w2 row reaches here only when w1_t = 0 (its box range is {0}),
    # i.e. eps_t is identically zero over the whole set: a crossing, and one
    # a zero-objective functional solve could not certify
    if (all(w2[t_row, ] == 0)) {
      cross <- c(cross, t_row)
      next
    }
    # relative slack biased toward flagging a crossing, so roundoff at the
    # functional endpoints never certifies a false negative
    slack <- PAPER_QUADRATIC_CONTROL$crossing_range_rtol *
      max(1, abs(w1[t_row]))
    fmin <- solve_linear_functional_bound(qs, w2[t_row, ], "min")
    if (!(fmin$bounded && fmin$valid)) {
      unresolved <- c(unresolved, t_row)
      next
    }
    if (w1[t_row] < fmin$bound - slack) next
    fmax <- solve_linear_functional_bound(qs, w2[t_row, ], "max")
    if (!(fmax$bounded && fmax$valid)) {
      unresolved <- c(unresolved, t_row)
    } else if (w1[t_row] <= fmax$bound + slack) {
      cross <- c(cross, t_row)
    }
  }
  list(cross = cross, unresolved = unresolved)
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

# scan theta_hat over the feasible grid in chunks: running per-coefficient
# extremes with their arg-extreme points, plus cross_grid -- the observations
# whose residual takes both signs (or an exact zero) across the feasible
# lattice, a sound crossing detector complementing the census. A NaN map
# value (a zero-weight times log(0) coincidence at a lattice point) is
# treated as missing; +/-Inf values are kept, since divergent sides are
# handled by the caller's crossing bookkeeping.
logvar_grid_scan <- function(
  b_feas,
  w1,
  w2,
  proj,
  chunk = LOGVAR_SEARCH_CONTROL$scan_chunk_size
) {
  stopifnot(nrow(b_feas) > 0L)
  n_coef <- nrow(proj)
  best_min <- rep(Inf, n_coef)
  best_max <- rep(-Inf, n_coef)
  arg_min <- arg_max <- matrix(NA_real_, n_coef, ncol(b_feas))
  any_nonpos <- rep(FALSE, length(w1))
  any_nonneg <- rep(FALSE, length(w1))
  for (s in seq(1L, nrow(b_feas), by = chunk)) {
    rows <- s:min(s + chunk - 1L, nrow(b_feas))
    eps <- w1 - w2 %*% t(b_feas[rows, , drop = FALSE])
    # rowSums comparisons are the C-level form of the both-signs tracker
    # (an exact zero satisfies both, so it still counts as a crossing)
    any_nonpos <- any_nonpos | (rowSums(eps <= 0) > 0)
    any_nonneg <- any_nonneg | (rowSums(eps >= 0) > 0)
    th <- proj %*% log(eps^2)
    th[is.nan(th)] <- NA
    for (j in seq_len(n_coef)) {
      k_min <- which.min(th[j, ])
      k_max <- which.max(th[j, ])
      if (length(k_min) && th[j, k_min] < best_min[j]) {
        best_min[j] <- th[j, k_min]
        arg_min[j, ] <- b_feas[rows[k_min], ]
      }
      if (length(k_max) && th[j, k_max] > best_max[j]) {
        best_max[j] <- th[j, k_max]
        arg_max[j, ] <- b_feas[rows[k_max], ]
      }
    }
  }
  list(
    min = best_min, max = best_max, arg_min = arg_min, arg_max = arg_max,
    cross_grid = which(any_nonpos & any_nonneg)
  )
}

# the endpoint polish (generalized objective seam plus the legacy
# logvar_polish_bound wrapper) lives in its own module so this file stays
# below the repository line cap; sourced here so existing callers see the
# same definitions
paper_source_once(paper_path("log_variance", "core", "endpoint_polish.R"))
