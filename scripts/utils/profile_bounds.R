# Non-dimensionalized profile-bounds solver with scale-aware unbounded detection
# and a solver-independent feasibility+active-constraint validity check.
# Invariance: theta = delta * phi preserves the feasible set for ANY delta > 0,
# and dividing each constraint g_i <= 0 by omega_i > 0 leaves it unchanged.

# Variable scale delta ~ natural theta length-scale sqrt(|c| / rho(A)).
.derive_theta_scale <- function(quadratic) {
  spectral <- vapply(quadratic$A_i, function(a) {
    max(abs(eigen((a + t(a)) / 2,
      symmetric = TRUE,
      only.values = TRUE
    )$values))
  }, numeric(1))
  a_ref <- stats::median(spectral)
  c_ref <- stats::median(abs(unlist(quadratic$c_i)))
  if (!is.finite(a_ref) || a_ref <= 0 ||
    !is.finite(c_ref) || c_ref <= 0) {
    return(1)
  }
  sqrt(c_ref / a_ref)
}

# Per-constraint normalization omega_i (positive) -> transformed g_i ~ O(1).
.derive_constraint_scales <- function(quadratic, delta) {
  vapply(seq_along(quadratic$A_i), function(i) {
    a <- (quadratic$A_i[[i]] + t(quadratic$A_i[[i]])) / 2
    rho <- max(abs(eigen(a,
      symmetric = TRUE,
      only.values = TRUE
    )$values)) * delta^2
    bmag <- sqrt(sum((delta * quadratic$b_i[[i]])^2))
    w <- max(rho, bmag, abs(quadratic$c_i[i]))
    if (!is.finite(w) || w <= 0) 1 else w
  }, numeric(1))
}

# Solve the scaled subproblem at a given box; return the FULL phi vector and the
# solver convergence code.
.solve_scaled <- function(quadratic, component_index, sign_mult, delta, omega,
                          box, xtol_rel, maxeval) {
  n_con <- length(quadratic$A_i)
  dim_theta <- ncol(quadratic$A_i[[1]])
  e_k <- numeric(dim_theta)
  e_k[component_index] <- 1
  obj_fn <- function(phi) sign_mult * sum(e_k * phi)
  grad_fn <- function(phi) sign_mult * e_k
  constraint_fn <- function(phi) {
    theta <- delta * phi
    vapply(seq_len(n_con), function(i) {
      (drop(t(theta) %*% quadratic$A_i[[i]] %*% theta) +
        sum(quadratic$b_i[[i]] * theta) + quadratic$c_i[i]) / omega[i]
    }, numeric(1))
  }
  constraint_jac <- function(phi) {
    theta <- delta * phi
    jac <- matrix(0, n_con, dim_theta)
    for (i in seq_len(n_con)) {
      jac[i, ] <- (delta * (2 * drop(quadratic$A_i[[i]] %*% theta) +
        quadratic$b_i[[i]])) / omega[i]
    }
    jac
  }
  res <- tryCatch(nloptr::slsqp(
    x0 = rep(0, dim_theta), fn = obj_fn, gr = grad_fn,
    lower = rep(-box, dim_theta), upper = rep(box, dim_theta),
    hin = constraint_fn, hinjac = constraint_jac,
    control = list(xtol_rel = xtol_rel, maxeval = maxeval),
    deprecatedBehavior = FALSE
  ), error = function(e) {
    list(par = rep(NA_real_, dim_theta), convergence = -99L)
  })
  list(phi = res$par, convergence = res$convergence)
}

# Most-binding normalized constraint value at theta (~0 at a feasible+active
# boundary point). Certifies FEASIBLE + at least one ACTIVE constraint -- it does
# NOT certify global optimality (no stationarity / multiplier check); on
# non-convex (indefinite A_i) sets a premature boundary stall can still pass.
.feasibility_residual <- function(quadratic, theta, omega) {
  max(vapply(seq_along(quadratic$A_i), function(i) {
    (drop(t(theta) %*% quadratic$A_i[[i]] %*% theta) +
      sum(quadratic$b_i[[i]] * theta) + quadratic$c_i[i]) / omega[i]
  }, numeric(1)))
}

# Profile bound (min or max of theta_k). Returns list(bound, bounded, valid):
#   (TRUE , TRUE ) finite feasible+active bound -> the value
#   (TRUE , FALSE) finite but feasibility check failed -> "unreliable"
#   (FALSE, TRUE ) unbounded: strictly feasible at box2 with NO active constraint
#   (FALSE, FALSE) solver failure / infeasible runaway -> NA (fail closed)
# Unboundedness is detected by the ABSENCE of a binding constraint at box2 (the
# residual stays << 0), NOT by phi reaching the box edge. With a LINEAR objective
# the only thing that can stop the optimum while it is strictly feasible is the
# box itself, so "no constraint binds" => unbounded -- and this holds even when a
# badly scaled ray stalls partway through the box instead of reaching the edge.
# Caveat (shared with .feasibility_residual): on a non-convex (indefinite A_i)
# set a strictly-interior stall short of a constraint that WOULD bind is read as
# unbounded here, just as a premature boundary stall can pass the active check.
solve_profile_bound <- function(quadratic, component_index,
                                direction = c("min", "max"),
                                box1 = 1e6, box2 = 1e9, feas_tol = 1e-4,
                                xtol_rel = 1e-8, maxeval = 1000) {
  direction <- match.arg(direction)
  delta <- .derive_theta_scale(quadratic)
  omega <- .derive_constraint_scales(quadratic, delta)
  sign_mult <- if (direction == "min") 1 else -1
  inf_bound <- if (direction == "min") -Inf else Inf

  r1 <- .solve_scaled(
    quadratic, component_index, sign_mult, delta, omega,
    box1, xtol_rel, maxeval
  )
  phi1k <- r1$phi[component_index]
  if (is.finite(phi1k) && abs(phi1k) < 0.5 * box1) {
    phi <- r1$phi
  } else {
    r2 <- .solve_scaled(
      quadratic, component_index, sign_mult, delta, omega,
      box2, xtol_rel, maxeval
    )
    phi2k <- r2$phi[component_index]
    if (!is.finite(phi2k)) {
      return(list(bound = NA_real_, bounded = FALSE, valid = FALSE))
    }
    resid2 <- .feasibility_residual(quadratic, delta * r2$phi, omega)
    if (!is.finite(resid2) || resid2 > feas_tol) {
      # Infeasible runaway -> fail closed.
      return(list(bound = NA_real_, bounded = FALSE, valid = FALSE))
    }
    if (resid2 < -feas_tol) {
      # Strictly feasible at box2: no constraint binds, so only the box stopped
      # the linear objective -> unbounded in this direction (regardless of where
      # in the box the solver stalled).
      return(list(bound = inf_bound, bounded = FALSE, valid = TRUE))
    }
    # |resid2| <= feas_tol: a constraint binds at box2 -> a genuine large finite
    # bound that box1 was simply too small to reach.
    phi <- r2$phi
  }
  resid <- .feasibility_residual(quadratic, delta * phi, omega)
  list(
    bound = delta * phi[component_index], bounded = TRUE,
    valid = is.finite(resid) && abs(resid) <= feas_tol
  )
}

# Profile bounds for every coordinate. width = Inf/NA when a side is unbounded/failed.
solve_all_profile_bounds <- function(quadratic, ...) {
  n_comp <- ncol(quadratic$A_i[[1]])
  rows <- lapply(seq_len(n_comp), function(k) {
    lo <- solve_profile_bound(quadratic, k, "min", ...)
    hi <- solve_profile_bound(quadratic, k, "max", ...)
    data.frame(
      component = k,
      lower = lo$bound, upper = hi$bound,
      width = hi$bound - lo$bound,
      bounded_lower = lo$bounded, bounded_upper = hi$bounded,
      valid_lower = lo$valid, valid_upper = hi$valid,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Closed-form point identification (tau = 0): each constraint is the perfect
# square (Q_i' theta - L_i)^2 <= 0, so the feasible set is the single point
# solving Q theta = L. Returns list(theta, cond) when the linear system is
# consistent, else NULL (caller falls back to the inequality solver).
solve_point_identification <- function(components, tol = 1e-8) {
  qmat <- do.call(rbind, components$Q_i)
  lvec <- components$L_i
  # Require FULL COLUMN RANK: an under-/rank-deficient system has solution set =
  # an affine subspace (unbounded), which qr.solve would silently collapse to a
  # single min-norm point and mislabel as width-0 point identification.
  if (nrow(qmat) < ncol(qmat) ||
    qr(qmat, tol = tol)$rank < ncol(qmat)) {
    return(NULL)
  }
  pt <- tryCatch(qr.solve(qmat, lvec), error = function(e) NULL)
  if (is.null(pt) || any(!is.finite(pt))) {
    return(NULL)
  }
  if (max(abs(qmat %*% pt - lvec)) > tol * max(1, max(abs(lvec)))) {
    return(NULL)
  }
  list(theta = as.numeric(pt), cond = kappa(qmat))
}
