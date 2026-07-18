# Facet linear programs for the Harvey recession certificate: the primal
# facet minimum, its dual with the projected-multiplier conservative bound,
# and the phase-I feasibility pair. Every certificate decision uses the
# recomputed stationarity residual and support correction, never a raw
# optimizer objective or return code. Definitions only; sourced by
# recession_certificate.R.

# solve min fn = obj'x over box [lo, hi] with hin(x) <= 0 rows gmat x - hvec
# and equalities emat x = evec, via the shared nloptr SLSQP; returns the
# point and nothing else -- callers recompute every certificate quantity
.logvar_lp_solve <- function(
  obj, gmat, hvec, emat, evec, lo, hi, x0,
  control = LOGVAR_HARVEY_CONTROL
) {
  res <- tryCatch(
    nloptr::slsqp(
      x0 = x0,
      fn = function(x) sum(obj * x),
      gr = function(x) obj,
      lower = lo, upper = hi,
      hin = function(x) drop(gmat %*% x) - hvec,
      hinjac = function(x) gmat,
      heq = function(x) drop(emat %*% x) - evec,
      heqjac = function(x) emat,
      control = list(
        xtol_rel = control$lp_xtol_rel,
        maxeval = control$lp_maxeval
      ),
      deprecatedBehavior = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(res) || any(!is.finite(res$par))) {
    return(NULL)
  }
  res$par
}

# the primal facet problem: min colSums(Z)' u over Z_p u >= 0, u_j = sign,
# -1 <= u <= 1; returns the candidate direction and its feasibility record
logvar_harvey_facet_primal <- function(
  c_vec, z_pos, j, sign_j,
  control = LOGVAR_HARVEY_CONTROL
) {
  p <- length(c_vec)
  x0 <- numeric(p)
  x0[j] <- sign_j
  gmat <- -z_pos
  hvec <- rep(0, nrow(z_pos))
  emat <- matrix(0, 1, p)
  emat[1, j] <- 1
  u <- .logvar_lp_solve(
    c_vec, gmat, hvec, emat, sign_j, rep(-1, p), rep(1, p), x0,
    control
  )
  if (is.null(u)) {
    return(list(ok = FALSE))
  }
  viol <- max(c(drop(gmat %*% u) - hvec, abs(u[j] - sign_j), abs(u) - 1))
  list(ok = TRUE, u = u, value = sum(c_vec * u), feas_viol = viol)
}

# the dual of the facet problem written over the FULL inequality system
# G = rbind(-Z_p, I, -I), h = c(0, 1, 1): max -h'lambda - sign*nu subject to
# c + G'lambda + E'nu = 0, lambda >= 0. The certificate uses only the
# projected multipliers with the recomputed stationarity residual:
# L = -h'lambda_plus - sign*nu - sum(abs(q)), the exact support correction
# implied by the unit box
logvar_harvey_facet_dual <- function(
  c_vec, z_pos, j, sign_j,
  control = LOGVAR_HARVEY_CONTROL
) {
  p <- length(c_vec)
  n_pos <- nrow(z_pos)
  gmat <- rbind(-z_pos, diag(p), -diag(p))
  hvec <- c(rep(0, n_pos), rep(1, 2 * p))
  m <- nrow(gmat)
  e_j <- numeric(p)
  e_j[j] <- 1
  # variables (lambda, nu): minimize h'lambda + sign*nu under stationarity
  obj <- c(hvec, sign_j)
  emat <- cbind(t(gmat), e_j)
  sol <- .logvar_lp_solve(
    obj, matrix(0, 1, m + 1), 0, emat, -c_vec,
    c(rep(0, m), -control$lp_bound),
    rep(control$lp_bound, m + 1),
    rep(0, m + 1),
    control
  )
  if (is.null(sol)) {
    return(list(ok = FALSE))
  }
  lambda_raw <- sol[seq_len(m)]
  nu <- sol[m + 1]
  lambda_plus <- pmax(lambda_raw, 0)
  q <- c_vec + drop(t(gmat) %*% lambda_plus) + e_j * nu
  list(
    ok = TRUE, lambda_raw = lambda_raw, lambda_plus = lambda_plus, nu = nu,
    stationarity_residual = q, support_correction = sum(abs(q)),
    bound = -sum(hvec * lambda_plus) - sign_j * nu - sum(abs(q))
  )
}

# phase-I pair for an apparently infeasible facet: minimize t subject to
# -Z_p u <= t, u_j = sign, -1 <= u <= 1, 0 <= t <= t_max; its dual bound
# L_I adds the t-residual correction t_max * |q_I[t]|
logvar_harvey_facet_phase1 <- function(
  z_pos, j, sign_j, t_max,
  control = LOGVAR_HARVEY_CONTROL
) {
  p <- ncol(z_pos)
  n_pos <- nrow(z_pos)
  obj <- c(numeric(p), 1)
  gmat <- cbind(-z_pos, -1)
  hvec <- rep(0, n_pos)
  emat <- matrix(0, 1, p + 1)
  emat[1, j] <- 1
  x0 <- c(numeric(p), t_max)
  x0[j] <- sign_j
  v <- .logvar_lp_solve(
    obj, gmat, hvec, emat, sign_j,
    c(rep(-1, p), 0), c(rep(1, p), t_max), x0, control
  )
  if (is.null(v)) {
    return(list(ok = FALSE))
  }
  t_min <- v[p + 1]
  # dual over the full phase-I inequality system, including both box blocks
  # and the explicit t bounds
  g_full <- rbind(
    cbind(-z_pos, -1),
    cbind(diag(p), numeric(p)), cbind(-diag(p), numeric(p)),
    c(numeric(p), 1), c(numeric(p), -1)
  )
  h_full <- c(rep(0, n_pos), rep(1, 2 * p), t_max, 0)
  m <- nrow(g_full)
  e_full <- emat[1, ]
  dual <- .logvar_lp_solve(
    c(h_full, sign_j), matrix(0, 1, m + 1), 0,
    cbind(t(g_full), e_full), -obj,
    c(rep(0, m), -control$lp_bound),
    rep(control$lp_bound, m + 1),
    rep(0, m + 1),
    control
  )
  if (is.null(dual)) {
    return(list(ok = TRUE, t_min = t_min, v = v, dual_ok = FALSE))
  }
  lambda_plus <- pmax(dual[seq_len(m)], 0)
  nu <- dual[m + 1]
  q <- obj + drop(t(g_full) %*% lambda_plus) + e_full * nu
  list(
    ok = TRUE, t_min = t_min, v = v, dual_ok = TRUE, t_max = t_max,
    lambda_raw = dual[seq_len(m)], lambda_plus = lambda_plus, nu = nu,
    stationarity_residual = q,
    bound = -sum(h_full * lambda_plus) - sign_j * nu -
      sum(abs(q[seq_len(p)])) - t_max * abs(q[p + 1])
  )
}
