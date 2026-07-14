# Endpoint polish for the log-variance map, generalized to pluggable
# objectives: logvar_polish_objective extremizes an arbitrary scalar fn(b)
# (optional gradient gr(b), both in theta units) over the mean-equation
# quadratic constraints in the shared solver's scaling, via SLSQP for smooth
# objectives with gradients or derivative-free COBYLA under the identical
# normalized constraints (house hin <= 0, deprecatedBehavior = FALSE; the
# installed nloptr::cobyla accepts it). Unlike the linear profile bounds, a
# nonlinear extremum can sit strictly inside the set, so the endpoint
# certificate is feasibility only (resid <= feas_tol), not
# feasibility+activity. logvar_polish_bound keeps the benchmark's exact
# signature and return as a thin wrapper passing the log-OLS closures.
# Definitions only; sourced by log_var_eq_map.R.

# Returns list(bound, par, feas_resid, suspect, convergence): bound and par are NULL when
# the solve fails or the endpoint is infeasible; suspect = TRUE (with a NULL
# bound) when the polished value explodes past blow_factor x the guard scale
# -- the optimizer diving toward a residual-zero singularity or an
# uncertifiably wild side.
logvar_polish_objective <- function(qs, direction, b_start, guard_scale,
                                    fn, gr = NULL,
                                    method = c("slsqp", "cobyla"),
                                    box = 1e6, feas_tol = 1e-4,
                                    blow_factor = 5) {
  method <- match.arg(method)
  # a non-finite scale would disable the blow guard (or make it error)
  if (!is.finite(guard_scale)) guard_scale <- 1
  delta <- .derive_theta_scale(qs)
  omega <- .derive_constraint_scales(qs, delta)
  sgn <- if (direction == "min") 1 else -1
  dim_b <- ncol(qs$A_i[[1]])
  x0 <- pmin(pmax(b_start / delta, -box), box)
  obj_fn <- function(phi) sgn * fn(delta * phi)
  hin_fn <- function(phi) {
    b <- delta * phi
    vapply(seq_along(qs$A_i), function(i) {
      (drop(t(b) %*% qs$A_i[[i]] %*% b) +
        sum(qs$b_i[[i]] * b) + qs$c_i[i]) / omega[i]
    }, numeric(1))
  }
  res <- tryCatch(
    if (method == "slsqp") {
      nloptr::slsqp(
        x0 = x0,
        fn = obj_fn,
        gr = function(phi) sgn * delta * gr(delta * phi),
        lower = rep(-box, dim_b), upper = rep(box, dim_b),
        hin = hin_fn,
        hinjac = function(phi) {
          b <- delta * phi
          t(vapply(seq_along(qs$A_i), function(i) {
            (delta * (2 * drop(qs$A_i[[i]] %*% b) + qs$b_i[[i]])) / omega[i]
          }, numeric(dim_b)))
        },
        control = list(xtol_rel = 1e-8, maxeval = 1000),
        deprecatedBehavior = FALSE
      )
    } else {
      nloptr::cobyla(
        x0 = x0,
        fn = obj_fn,
        lower = rep(-box, dim_b), upper = rep(box, dim_b),
        hin = hin_fn,
        control = list(xtol_rel = 1e-8, maxeval = 1000),
        deprecatedBehavior = FALSE
      )
    },
    error = function(e) NULL
  )
  out <- function(bound, par, feas_resid, suspect) {
    list(
      bound = bound, par = par, feas_resid = feas_resid, suspect = suspect,
      convergence = if (is.null(res)) NA_integer_ else res$convergence
    )
  }
  if (is.null(res) || any(!is.finite(res$par))) {
    return(out(NULL, NULL, NA_real_, FALSE))
  }
  b_pol <- delta * res$par
  resid <- .feasibility_residual(qs, b_pol, omega)
  if (!is.finite(resid) || resid > feas_tol) {
    return(out(NULL, b_pol, resid, FALSE))
  }
  bound <- fn(b_pol)
  if (!is.finite(bound) || abs(bound) > blow_factor * max(1, guard_scale)) {
    return(out(NULL, b_pol, resid, TRUE))
  }
  out(bound, b_pol, resid, FALSE)
}

# SLSQP polish of one theta_hat side from a feasible start (the benchmark
# path): the log-OLS objective and gradient closures passed through the
# generalized seam above, with the legacy list(bound, suspect) return
logvar_polish_bound <- function(qs, direction, b_start, grid_scale,
                                w1, w2, proj_row,
                                box = 1e6, feas_tol = 1e-4, blow_factor = 5) {
  pol <- logvar_polish_objective(
    qs, direction, b_start, grid_scale,
    fn = function(b) sum(proj_row * log(drop(w1 - w2 %*% b)^2)),
    gr = function(b) logvar_theta_grad(b, w1, w2, proj_row),
    method = "slsqp", box = box, feas_tol = feas_tol,
    blow_factor = blow_factor
  )
  list(bound = pol$bound, suspect = pol$suspect)
}
