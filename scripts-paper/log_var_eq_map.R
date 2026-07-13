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
# sourced by
# log_var_eq.R after profile_bounds_core.R (.derive_* / .feasibility_residual
# internals) and profile_bounds.R (solve_linear_functional_bound), and by
# scripts/utils/tests/test_logvar_map.R.

# projection rows P = (R'R)^{-1} R' of the log-variance regression; row j
# gives theta_hat_j(b) = P[j, ] %*% log((w1 - W2 b)^2)
logvar_projection <- function(pcr) {
  r_mat <- cbind("(Intercept)" = 1, pcr)
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

# residual-zero census over the joint set: observation t's hyperplane
# w2_t' b = w1_t intersects the set iff w1_t lies inside the range of the
# linear functional w2_t' b over it. The closed-form range over the bounding
# box is a sound outer screen (outside it, no crossing is possible); each
# remaining ambiguous row gets functional bounds over the set itself, sound
# for found crossings and solver-certified for no-crossing verdicts. cross
# collects the certified crossings, unresolved the rows whose solves failed
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
    slack <- 1e-8 * max(1, abs(w1[t_row]))
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
# set; 1e-10 is far inside the solvers' 1e-4 feasibility certificate)
logvar_feasible_grid <- function(qs, lower, upper, n_axis) {
  axes <- Map(function(lo, hi) seq(lo, hi, length.out = n_axis), lower, upper)
  b_grid <- as.matrix(expand.grid(axes, KEEP.OUT.ATTRS = FALSE))
  dimnames(b_grid) <- NULL
  omega <- .derive_constraint_scales(qs, .derive_theta_scale(qs))
  feas <- rep(TRUE, nrow(b_grid))
  for (i in seq_along(qs$A_i)) {
    g <- rowSums((b_grid %*% qs$A_i[[i]]) * b_grid) +
      drop(b_grid %*% qs$b_i[[i]]) + qs$c_i[i]
    feas <- feas & (g <= 1e-10 * omega[i])
  }
  b_grid[feas, , drop = FALSE]
}

# scan theta_hat over the feasible grid in chunks: running per-coefficient
# extremes with their arg-extreme points, plus cross_grid -- the observations
# whose residual takes both signs (or an exact zero) across the feasible
# lattice, a sound crossing detector complementing the census. A NaN map
# value (a zero-weight times log(0) coincidence at a lattice point) is
# treated as missing; +/-Inf values are kept, since divergent sides are
# handled by the caller's crossing bookkeeping.
logvar_grid_scan <- function(b_feas, w1, w2, proj, chunk = 5000L) {
  stopifnot(nrow(b_feas) > 0L)
  n_coef <- nrow(proj)
  best_min <- rep(Inf, n_coef)
  best_max <- rep(-Inf, n_coef)
  arg_min <- arg_max <- matrix(NA_real_, n_coef, ncol(b_feas))
  sign_lo <- rep(1, length(w1))
  sign_hi <- rep(-1, length(w1))
  for (s in seq(1L, nrow(b_feas), by = chunk)) {
    rows <- s:min(s + chunk - 1L, nrow(b_feas))
    eps <- w1 - w2 %*% t(b_feas[rows, , drop = FALSE])
    eps_sign <- sign(eps)
    sign_lo <- pmin(sign_lo, apply(eps_sign, 1, min))
    sign_hi <- pmax(sign_hi, apply(eps_sign, 1, max))
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
    cross_grid = which(sign_lo * sign_hi <= 0)
  )
}

# SLSQP polish of one theta_hat side from a feasible start, in the shared
# solver's scaling (same normalized-constraint pattern as
# set_id_bounds_tau.R). Unlike the linear profile bounds, a nonlinear
# extremum can sit strictly inside the set, so the endpoint certificate is
# feasibility only (resid <= feas_tol), not feasibility+activity. Returns
# list(bound, suspect): bound is NULL when the solve fails or the endpoint
# is infeasible; suspect = TRUE (with a NULL bound) when the polished value
# explodes past blow_factor x the grid scale -- the optimizer diving toward
# a residual-zero singularity or an uncertifiably wild side.
logvar_polish_bound <- function(qs, direction, b_start, grid_scale,
                                w1, w2, proj_row,
                                box = 1e6, feas_tol = 1e-4, blow_factor = 5) {
  # a non-finite scale would disable the blow guard (or make it error)
  if (!is.finite(grid_scale)) grid_scale <- 1
  delta <- .derive_theta_scale(qs)
  omega <- .derive_constraint_scales(qs, delta)
  sgn <- if (direction == "min") 1 else -1
  dim_b <- ncol(qs$A_i[[1]])
  res <- tryCatch(
    nloptr::slsqp(
      x0 = pmin(pmax(b_start / delta, -box), box),
      fn = function(phi) {
        sgn * sum(proj_row * log(drop(w1 - w2 %*% (delta * phi))^2))
      },
      gr = function(phi) {
        sgn * delta * logvar_theta_grad(delta * phi, w1, w2, proj_row)
      },
      lower = rep(-box, dim_b), upper = rep(box, dim_b),
      hin = function(phi) {
        b <- delta * phi
        vapply(seq_along(qs$A_i), function(i) {
          (drop(t(b) %*% qs$A_i[[i]] %*% b) +
            sum(qs$b_i[[i]] * b) + qs$c_i[i]) / omega[i]
        }, numeric(1))
      },
      hinjac = function(phi) {
        b <- delta * phi
        t(vapply(seq_along(qs$A_i), function(i) {
          (delta * (2 * drop(qs$A_i[[i]] %*% b) + qs$b_i[[i]])) / omega[i]
        }, numeric(dim_b)))
      },
      control = list(xtol_rel = 1e-8, maxeval = 1000),
      deprecatedBehavior = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(res) || any(!is.finite(res$par))) {
    return(list(bound = NULL, suspect = FALSE))
  }
  b_pol <- delta * res$par
  resid <- .feasibility_residual(qs, b_pol, omega)
  if (!is.finite(resid) || resid > feas_tol) {
    return(list(bound = NULL, suspect = FALSE))
  }
  bound <- sum(proj_row * log(drop(w1 - w2 %*% b_pol)^2))
  if (!is.finite(bound) || abs(bound) > blow_factor * max(1, grid_scale)) {
    return(list(bound = NULL, suspect = TRUE))
  }
  list(bound = bound, suspect = FALSE)
}
