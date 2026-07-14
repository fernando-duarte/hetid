# Conditional L-infinity (sup-norm) epigraph sensitivity for the joint-null
# diagnostic, sourced by log_var_eq_joint_null_search.R. When the pinned trigger
# fires (owned by the at-tau orchestration), the smallest attained scaled
# sup-norm d_inf = max_j |s_j / d_j| is refined by the epigraph program
#   min r  s.t.  s_j / d_j <= r,  -s_j / d_j <= r,  r >= 0,  b in the Lewbel set
# started from the two already-admitted candidates (the L2 winner and the best
# grid sup-norm point). Derivative-free COBYLA (NLOPT_LN_COBYLA, house hin <= 0,
# deprecatedBehavior = FALSE) in normalized (b / b_scale, r / r_scale)
# coordinates; the returned map, r, and every constraint are recomputed directly
# and a failed or unverified solve falls back to the better admitted start. A
# sensitivity read only -- never a headline bound or a global certificate.

# scaled slope components s_j / d_j = s_j * sqrt(d_inv2) at b (length 4)
.jn_scaled_slopes <- function(b, w1, w2, proj, d_inv2) {
  as.numeric(logvar_joint_null_objective(b, w1, w2, proj, d_inv2)$s) * sqrt(d_inv2)
}

# Shared scan helper kept here so the search module stays under the line cap:
# individual points (tau = 0 seeds, carried prior minima) evaluated through the
# map, keeping the qs-feasible finite-q ones tagged with their provenance.
.jn_extra_candidates <- function(points, type, qs, w1, w2, proj, d_inv2) {
  if (!length(points)) {
    return(list())
  }
  omega <- .jn_omega(qs)
  out <- list()
  for (pt in points) {
    b <- as.numeric(pt)
    resid <- .feasibility_residual(qs, b, omega)
    if (!is.finite(resid) || resid > 1e-4) next
    o <- logvar_joint_null_objective(b, w1, w2, proj, d_inv2)
    if (!is.finite(o$q)) next
    out[[length(out) + 1L]] <- list(
      b = b, q = as.numeric(o$q), s = as.numeric(o$s), type = type
    )
  }
  out
}

# COBYLA epigraph solve from one admitted start; NULL unless the recomputed
# point is Lewbel-feasible and every epigraph inequality holds to tolerance
.jn_epigraph_solve <- function(b0, qs, omega, b_scale, r_scale,
                               w1, w2, proj, d_inv2) {
  k <- length(b0)
  r0 <- max(abs(.jn_scaled_slopes(b0, w1, w2, proj, d_inv2)))
  hin <- function(x) {
    b <- b_scale * x[seq_len(k)]
    r <- r_scale * x[k + 1L]
    lew <- vapply(seq_along(qs$A_i), function(i) {
      (drop(t(b) %*% qs$A_i[[i]] %*% b) + sum(qs$b_i[[i]] * b) + qs$c_i[i]) /
        omega[i]
    }, numeric(1))
    sc <- .jn_scaled_slopes(b, w1, w2, proj, d_inv2)
    c(lew, -x[k + 1L], (sc - r) / r_scale, (-sc - r) / r_scale)
  }
  res <- tryCatch(
    nloptr::cobyla(
      x0 = c(b0 / b_scale, r0 / r_scale), fn = function(x) x[k + 1L],
      lower = rep(-1e6, k + 1L), upper = rep(1e6, k + 1L), hin = hin,
      control = list(xtol_rel = 1e-10, maxeval = 500L),
      deprecatedBehavior = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(res) || any(!is.finite(res$par))) {
    return(NULL)
  }
  b_pol <- b_scale * res$par[seq_len(k)]
  r_pol <- r_scale * res$par[k + 1L]
  d_inf <- max(abs(.jn_scaled_slopes(b_pol, w1, w2, proj, d_inv2)))
  feas <- .feasibility_residual(qs, b_pol, omega)
  ok <- is.finite(feas) && feas <= 1e-4 && is.finite(d_inf) &&
    is.finite(r_pol) && r_pol >= -1e-8 && d_inf <= r_pol + 1e-8 * max(1, r_pol)
  if (!ok) {
    return(NULL)
  }
  list(
    b = b_pol, r = max(r_pol, 0), d_inf = d_inf, feas_resid = feas,
    status = "polished"
  )
}

# Refine the attained sup-norm from both admitted starts, returning the better
# admitted start (status "attained_start_only") when neither solve verifies.
# starts is the two-element list (L2 winner, best grid sup-norm candidate), each
# already a feasible attained point.
logvar_joint_null_epigraph <- function(qs, starts, w1, w2, proj, d_inv2,
                                       root_tol = 1e-6) {
  omega <- .jn_omega(qs)
  delta <- .derive_theta_scale(qs)
  r_start <- vapply(starts, function(b) {
    max(abs(.jn_scaled_slopes(as.numeric(b), w1, w2, proj, d_inv2)))
  }, numeric(1))
  better <- which.min(r_start)
  b_better <- as.numeric(starts[[better]])
  best <- list(
    b = b_better, r = r_start[better], d_inf = r_start[better],
    feas_resid = .feasibility_residual(qs, b_better, omega),
    status = "attained_start_only"
  )
  b_scale <- rep(delta, length(b_better))
  r_scale <- max(root_tol, min(r_start))
  for (s0 in starts) {
    res <- .jn_epigraph_solve(
      as.numeric(s0), qs, omega, b_scale, r_scale, w1, w2, proj, d_inv2
    )
    if (!is.null(res) && res$d_inf < best$d_inf) {
      best <- res
    }
  }
  best
}
