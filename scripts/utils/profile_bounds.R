# Profile-bounds API: the coordinate-tracking bound solver, the all-coordinates
# wrapper, and closed-form tau = 0 point identification. The scaled-solve
# internals live in profile_bounds_core.R, which must be sourced first.

# Profile bound (min or max of theta_k). Returns list(bound, bounded, valid):
#   (TRUE , TRUE ) finite feasible+active bound -> the value
#   (TRUE , FALSE) finite but feasibility/convergence check failed -> "unreliable"
#   (FALSE, TRUE ) unbounded -> +/-Inf
#   (FALSE, FALSE) solver failure / infeasible at the largest box / target value
#                  never certified stable -> NA (fail closed)
#
# Boundedness is decided by COORDINATE TRACKING across growing boxes, NOT by which
# constraint happens to be active: a genuine finite bound sits at a fixed location
# (interior to a large-enough box), while an unbounded coordinate rides the box
# edge at every box. The box1 solution is accepted directly only when NO
# coordinate rode the box (so an unbounded ray in a non-target coordinate cannot
# masquerade as a finite bound). At the enlarged boxes (box2, box3) a solve is
# accepted outright only when EVERY coordinate lands strictly interior; when the
# target is interior but some OTHER coordinate rides the edge, the target value
# is trusted only once it has STOPPED MOVING as the box grows (a constraint pins
# it even though the set is unbounded elsewhere). A target value that scales up
# with the box is an oblique unbounded ray (e.g. theta_k <= a * theta_j with
# theta_j free) and is reported unbounded, never as a finite bound; a target
# interior at the last box whose stability was never certified fails closed.
# Caveat: on a non-convex (indefinite A_i) set a strictly-interior local stall
# short of a constraint that WOULD bind can still be misread -- inherent to the
# local solver.
solve_profile_bound <- function(quadratic, component_index,
                                direction = c("min", "max"),
                                box1 = 1e6, box2 = 1e9, box3 = 1e10,
                                feas_tol = 1e-4,
                                xtol_rel = 1e-8, maxeval = 1000) {
  direction <- match.arg(direction)
  # The analytic constraint Jacobian (2*A%*%theta + b in .solve_scaled) is the
  # gradient of theta'A theta only for symmetric A; the general gradient is
  # (A+t(A))%*%theta. Guard here so direct callers and solve_all_profile_bounds
  # are both covered.
  for (a in quadratic$A_i) {
    if (max(abs(a - t(a))) > 1e-8 * max(1, max(abs(a)))) {
      stop(
        "A_i must be symmetric; symmetrize as (A+t(A))/2 before solving -- ",
        "the analytic SLSQP Jacobian assumes symmetry"
      )
    }
  }
  delta <- .derive_theta_scale(quadratic)
  omega <- .derive_constraint_scales(quadratic, delta)
  sign_mult <- if (direction == "min") 1 else -1
  inf_bound <- if (direction == "min") -Inf else Inf

  r1 <- .solve_scaled(
    quadratic, component_index, sign_mult, delta, omega,
    box1, xtol_rel, maxeval
  )
  # Fast path: accept box1 only if it is a clean solve with NO coordinate on the
  # box edge (an edge-riding coordinate -- target or not -- means the box, not a
  # constraint, stopped the optimum, so fall through to coordinate tracking).
  if (.solve_finite(r1) && all(abs(r1$phi) < 0.99 * box1)) {
    return(.finalize_bound(quadratic, delta, omega, r1, component_index, feas_tol))
  }

  # Enlarge the box; a genuine finite bound shows up interior to a big-enough box,
  # an unbounded direction keeps riding the edge. bound_prev tracks the target
  # value across boxes for the interior-target-with-edge-riding-coordinates case.
  bound_prev <- NULL
  target_interior <- FALSE
  for (bx in c(box2, box3)) {
    r <- .solve_scaled(
      quadratic, component_index, sign_mult, delta, omega,
      bx, xtol_rel, maxeval
    )
    if (!.solve_finite(r)) {
      return(list(bound = NA_real_, bounded = FALSE, valid = FALSE))
    }
    resid <- .feasibility_residual(quadratic, delta * r$phi, omega)
    if (!is.finite(resid) || resid > feas_tol) {
      # No feasible point at this box (empty set / runaway) -> fail closed.
      return(list(bound = NA_real_, bounded = FALSE, valid = FALSE))
    }
    if (all(abs(r$phi) < 0.99 * bx)) {
      # Every coordinate interior -> a constraint stopped the solve -> accept.
      return(.finalize_bound(quadratic, delta, omega, r, component_index, feas_tol))
    }
    target_interior <- abs(r$phi[component_index]) < 0.99 * bx
    if (!target_interior) {
      # Target itself rides the edge: no trustworthy value to track.
      bound_prev <- NULL
      next
    }
    # Target interior while another coordinate rides the edge: the target value
    # is trustworthy only if it has stopped moving as the box grows.
    bound_now <- delta * r$phi[component_index]
    if (!is.null(bound_prev)) {
      if (abs(bound_now - bound_prev) <= 1e-3 * max(1, abs(bound_now))) {
        # Stable across boxes -> a constraint pins the target even though the
        # set is unbounded in another direction -> genuine finite bound.
        return(.finalize_bound(quadratic, delta, omega, r, component_index, feas_tol))
      }
      if (abs(bound_now) >= 5 * max(abs(bound_prev), delta)) {
        # Target scales with the box -> oblique unbounded ray.
        return(list(bound = inf_bound, bounded = FALSE, valid = TRUE))
      }
    }
    bound_prev <- bound_now
  }
  if (!target_interior) {
    # Target rode the edge even at box3 -> unbounded in this direction.
    return(list(bound = inf_bound, bounded = FALSE, valid = TRUE))
  }
  # Target was interior at the last box but its stability was never certified
  # (and it did not clearly scale with the box) -> fail closed.
  list(bound = NA_real_, bounded = FALSE, valid = FALSE)
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
