# The bespoke SLSQP epigraph solver for the joint log-variance GMM Stage B
# (Plan 4, logvar-joint-gmm, dossier section 6.2). The exact minimax
#   min r  s.t.  +/- S^{-1} g(x) - r <= 0,  Lewbel(x) <= 0,  smooth sign guards,
#   r >= 0
# is solved in frozen normalized coordinates u = (x - center) / scale with the
# house hin <= 0 convention and an analytic constraint Jacobian
# cbind(+/- S^{-1} J_g diag(scale), -1); omitting the Jacobian would silently
# drop SLSQP to finite differences through the crossing singularities. The
# objective gradient is c(0, ..., 0, 1). Fresh acceptance recomputes every
# constraint and demands the exact raw sign s_i e_i(x) > 0 on each guarded row.
# The direction hook certifies recession only from an analytic record, never
# from finite probes. Definitions only; sourced by log_var_eq_joint_epigraph.R.

# Assemble the epigraph program in normalized coordinates: the linear objective
# on r, its gradient, the box bounds, and the hin/hinjac closures over the
# moment, Lewbel, and smooth sign-guard rows. Returned as closures so the same
# assembly serves the solver and the synthetic exact-Jacobian checks. The spec
# supplies center/scale, moment scales s_g, moment(x)/moment_jac(x) as an m x k
# matrix, and optional lewbel and guard blocks.
logvar_joint_epigraph_program <- function(spec, box_half_width, r_upper = 1e6) {
  k <- length(spec$center)
  center <- spec$center
  scale <- spec$scale
  make_x <- function(v) center + scale * v[seq_len(k)]
  hin <- function(v) {
    x <- make_x(v)
    r <- v[k + 1L]
    sg <- spec$moment(x) / spec$s_g
    rows <- c(sg - r, -sg - r)
    if (!is.null(spec$lewbel)) rows <- c(rows, spec$lewbel(x))
    if (!is.null(spec$guard)) {
      e <- spec$guard$resid(x)
      rows <- c(rows, 1 - spec$guard$signs * e / spec$guard$eps_floor)
    }
    rows
  }
  hinjac <- function(v) {
    x <- make_x(v)
    sjg <- sweep(spec$moment_jac(x) / spec$s_g, 2L, scale, "*")
    jac <- rbind(cbind(sjg, -1), cbind(-sjg, -1))
    if (!is.null(spec$lewbel)) {
      jac <- rbind(jac, cbind(sweep(spec$lewbel_jac(x), 2L, scale, "*"), 0))
    }
    if (!is.null(spec$guard)) {
      coef <- -(spec$guard$signs / spec$guard$eps_floor)
      jg <- sweep(coef * spec$guard$resid_jac(x), 2L, scale, "*")
      jac <- rbind(jac, cbind(jg, 0))
    }
    jac
  }
  list(
    k = k, fn = function(v) v[k + 1L], gr = function(v) c(rep(0, k), 1),
    lower = c(rep(-box_half_width, k), 0),
    upper = c(rep(box_half_width, k), r_upper),
    hin = hin, hinjac = hinjac, make_x = make_x
  )
}

# Fresh domain check at a raw point: Lewbel rows within constraint_tol and the
# exact raw sign on every guarded row. Absent blocks pass trivially.
logvar_joint_epigraph_domain_ok <- function(spec, x) {
  ctol <- logvar_joint_gmm_constants$constraint_tol
  lew_ok <- is.null(spec$lewbel) || all(spec$lewbel(x) <= ctol)
  sign_ok <- is.null(spec$guard) ||
    all(spec$guard$signs * spec$guard$resid(x) > 0)
  isTRUE(lew_ok) && isTRUE(sign_ok)
}

# Assemble a solve record with the fresh scaled sup-norm, box-interiority at
# box_tol, domain re-verification, and the resulting numerical status. Every
# finite scaled sup-norm is an attained upper bound on the global minimum.
logvar_joint_epigraph_result <- function(spec, x, u, r, box_half_width,
                                         box_tol, convergence, source) {
  finite_x <- all(is.finite(x))
  scaled_linf <- if (finite_x) max(abs(spec$moment(x) / spec$s_g)) else NA_real_
  interior <- finite_x &&
    all(abs(u) < box_half_width - box_tol * max(1, box_half_width))
  domain_ok <- finite_x && logvar_joint_epigraph_domain_ok(spec, x)
  attained <- finite_x && is.finite(scaled_linf) && domain_ok
  list(
    par = x, u = u, r = r, scaled_linf = scaled_linf, box_interior = interior,
    domain_ok = domain_ok, box_half_width = box_half_width,
    convergence = convergence, source = source,
    numerical_status = if (attained) "attained" else "unreliable"
  )
}

# Solve the epigraph from one raw-coordinate start x0 at one box width. Freshly
# initialize r0 from the scaled moment sup-norm, short-circuit an exact zero when
# the domain constraints already pass, otherwise run SLSQP and re-verify the
# returned point. A failed or unverified solve is numerical_status "unreliable".
logvar_joint_epigraph_solve <- function(spec, x0, box_half_width,
                                        box_tol = 1e-6) {
  k <- length(spec$center)
  prog <- logvar_joint_epigraph_program(spec, box_half_width)
  init <- logvar_joint_epigraph_start(spec$moment(x0) / spec$s_g)
  if (init$short_circuit && logvar_joint_epigraph_domain_ok(spec, x0)) {
    return(logvar_joint_epigraph_result(
      spec, x0, (x0 - spec$center) / spec$scale, 0, box_half_width, box_tol,
      0L, "short_circuit"
    ))
  }
  u0 <- (x0 - spec$center) / spec$scale
  solver_error <- NULL
  res <- tryCatch(
    nloptr::slsqp(
      x0 = c(u0, init$r_start), fn = prog$fn, gr = prog$gr,
      lower = prog$lower, upper = prog$upper, hin = prog$hin,
      hinjac = prog$hinjac,
      control = list(
        xtol_rel = logvar_joint_gmm_constants$param_xtol_rel,
        maxeval = logvar_joint_gmm_constants$per_solve_maxeval
      ),
      deprecatedBehavior = FALSE
    ),
    error = function(e) {
      solver_error <<- conditionMessage(e)
      NULL
    }
  )
  # surface the optimizer's own message rather than discarding it, so a structural
  # failure is diagnosable instead of hiding inside a generic solve_failed verdict
  if (!is.null(solver_error)) {
    warning("logvar_joint_epigraph_solve: SLSQP error -- ", solver_error)
  }
  if (is.null(res) || any(!is.finite(res$par))) {
    return(logvar_joint_epigraph_result(
      spec, x0, rep(NA_real_, k), NA_real_, box_half_width, box_tol,
      NA_integer_, "solve_failed"
    ))
  }
  u_star <- res$par[seq_len(k)]
  logvar_joint_epigraph_result(
    spec, spec$center + spec$scale * u_star, u_star, res$par[k + 1L],
    box_half_width, box_tol, res$convergence, "slsqp"
  )
}

# Direction-certificate hook. certified is TRUE only from an analytic recession
# record with a nonzero finite direction, a positive rate, and a +/- sign; the
# direction is returned normalized. Finite probes must call with
# analytic_proof = FALSE and thus can never certify.
logvar_joint_direction_certificate <- function(direction, rate, sign, record,
                                               analytic_proof) {
  ok <- isTRUE(analytic_proof) && is.numeric(direction) &&
    length(direction) > 0L && all(is.finite(direction)) &&
    any(direction != 0) && is.finite(rate) && rate > 0 && sign %in% c(-1, 1)
  dir_out <- if (ok) direction / sqrt(sum(direction^2)) else direction
  list(
    certified = isTRUE(ok), direction = dir_out, rate = rate, sign = sign,
    record = record
  )
}
