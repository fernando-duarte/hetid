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
# A RELATIVE floor rescues constraints whose scale is negligible versus the rest:
# without it, dividing a near-machine-epsilon constraint value by a microscopic
# omega_i blows the feasibility residual up to O(1) and spuriously trips the
# active/infeasible checks. The floor is relative to the median positive scale, so
# a uniformly small but well-scaled system (every omega_i tiny) is left untouched.
.derive_constraint_scales <- function(quadratic, delta) {
  raw <- vapply(seq_along(quadratic$A_i), function(i) {
    a <- (quadratic$A_i[[i]] + t(quadratic$A_i[[i]])) / 2
    rho <- max(abs(eigen(a,
      symmetric = TRUE,
      only.values = TRUE
    )$values)) * delta^2
    bmag <- sqrt(sum((delta * quadratic$b_i[[i]])^2))
    max(rho, bmag, abs(quadratic$c_i[i]))
  }, numeric(1))
  raw[!is.finite(raw)] <- 0
  pos <- raw[raw > 0]
  floor_val <- if (length(pos)) 1e-12 * stats::median(pos) else 0
  w <- pmax(raw, floor_val)
  w[!is.finite(w) | w <= 0] <- 1
  w
}

# Solve the scaled subproblem at a given box; return the FULL phi vector and the
# solver convergence code. With objective = NULL the objective is the coordinate
# functional e_k' phi (component bounds, byte-identical to the original path);
# with objective = c it is the linear functional c' phi (the theta-units value is
# delta * c' phi, recovered by .finalize_linear_bound). delta is a positive
# constant, so scaling the objective by it would not move the argmin; it is left
# out of the objective and applied only when recovering the bound, mirroring the
# coordinate convention (obj in phi units, bound in theta units).
.solve_scaled <- function(quadratic, component_index, sign_mult, delta, omega,
                          box, xtol_rel, maxeval, objective = NULL) {
  n_con <- length(quadratic$A_i)
  dim_theta <- ncol(quadratic$A_i[[1]])
  if (is.null(objective)) {
    e_k <- numeric(dim_theta)
    e_k[component_index] <- 1
    obj_fn <- function(phi) sign_mult * sum(e_k * phi)
    grad_fn <- function(phi) sign_mult * e_k
  } else {
    obj_fn <- function(phi) sign_mult * sum(objective * phi)
    grad_fn <- function(phi) sign_mult * objective
  }
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

# A scaled solve is usable for a bound/track decision only if every coordinate is
# finite (a crashed solve returns NA for all of phi).
.solve_finite <- function(r) all(is.finite(r$phi))

# Turn a finite scaled solution into a finite bound. VALID is the
# solver-INDEPENDENT feasible+active certificate (residual ~ 0): a feasible point
# that sits on at least one constraint boundary. We deliberately do NOT gate this
# on the solver's convergence code -- SLSQP returns roundoff-limited codes on
# legitimately ill-conditioned bounds, so trusting the residual certificate
# (with hard crashes already screened by .solve_finite) avoids false "unreliable"
# verdicts on otherwise valid bounds.
.finalize_bound <- function(quadratic, delta, omega, r, component_index, feas_tol) {
  resid <- .feasibility_residual(quadratic, delta * r$phi, omega)
  list(
    bound = delta * r$phi[component_index], bounded = TRUE,
    valid = is.finite(resid) && abs(resid) <= feas_tol
  )
}

# Linear-functional analog of .finalize_bound: the bound is the theta-units
# functional value delta * (objective_vec' phi) = c' theta; the valid certificate
# is the same solver-independent feasible+active residual check.
.finalize_linear_bound <- function(quadratic, delta, omega, r, objective_vec,
                                   feas_tol) {
  resid <- .feasibility_residual(quadratic, delta * r$phi, omega)
  list(
    bound = delta * sum(objective_vec * r$phi), bounded = TRUE,
    valid = is.finite(resid) && abs(resid) <= feas_tol
  )
}
