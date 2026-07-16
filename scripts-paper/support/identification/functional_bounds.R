# Exact min/max of an arbitrary linear functional c'theta over the identified
# set, reusing the coordinate solver's box-growth / feasibility / active-
# constraint machinery. Returns list(bound, bounded, valid) with the same
# semantics as solve_profile_bound, but the tracked target is the FUNCTIONAL
# value c'theta rather than a single coordinate theta_k. Used to bound an affine
# image of theta (e.g. a structural coefficient beta1_k(theta) = beta1R_k -
# c_k'theta) over the quadratically-constrained set without enumerating vertices.
# Boundedness logic (per the coordinate solver): a clean solve with NO coordinate
# on the box edge is accepted directly; at the enlarged boxes a solve with EVERY
# coordinate interior is accepted (a constraint stopped it); otherwise the
# functional value is trusted only once it STOPS MOVING as the box grows (a
# constraint pins it even though the set is unbounded in some free direction --
# the case of an objective orthogonal to that direction), while a functional
# value that SCALES with the box is an unbounded ray. There is no single target
# coordinate, so the coordinate-interior gate of solve_profile_bound is dropped
# in favour of functional-value tracking.
solve_linear_functional_bound <- function(quadratic, objective_vec,
                                          direction = c("min", "max"),
                                          box1 = 1e6, box2 = 1e9, box3 = 1e10,
                                          feas_tol = 1e-4,
                                          xtol_rel = 1e-8, maxeval = 1000) {
  direction <- match.arg(direction)
  for (a in quadratic$A_i) {
    if (max(abs(a - t(a))) > 1e-8 * max(1, max(abs(a)))) {
      stop(
        "A_i must be symmetric; symmetrize as (A+t(A))/2 before solving -- ",
        "the analytic SLSQP Jacobian assumes symmetry"
      )
    }
  }
  dim_theta <- ncol(quadratic$A_i[[1]])
  if (length(objective_vec) != dim_theta) {
    stop(
      "objective_vec length (", length(objective_vec),
      ") must equal the theta dimension (", dim_theta, ")"
    )
  }
  delta <- .derive_theta_scale(quadratic)
  omega <- .derive_constraint_scales(quadratic, delta)
  sign_mult <- if (direction == "min") 1 else -1
  inf_bound <- if (direction == "min") -Inf else Inf

  scaled <- function(box) {
    .solve_scaled(
      quadratic, NA_integer_, sign_mult, delta, omega,
      box, xtol_rel, maxeval,
      objective = objective_vec
    )
  }
  funcval <- function(r) delta * sum(objective_vec * r$phi)

  r1 <- scaled(box1)
  if (.solve_finite(r1) && all(abs(r1$phi) < BOX_EDGE_RTOL * box1)) {
    return(.finalize_linear_bound(quadratic, delta, omega, r1, objective_vec, feas_tol))
  }

  bound_prev <- NULL
  for (bx in c(box2, box3)) {
    r <- scaled(bx)
    if (!.solve_finite(r)) {
      return(list(bound = NA_real_, bounded = FALSE, valid = FALSE))
    }
    resid <- .feasibility_residual(quadratic, delta * r$phi, omega)
    if (!is.finite(resid) || resid > feas_tol) {
      return(list(bound = NA_real_, bounded = FALSE, valid = FALSE))
    }
    if (all(abs(r$phi) < BOX_EDGE_RTOL * bx)) {
      return(.finalize_linear_bound(quadratic, delta, omega, r, objective_vec, feas_tol))
    }
    # A coordinate rides the edge: the functional value is trustworthy only if it
    # has stopped moving (a constraint pins it) and is unbounded if it scales.
    bound_now <- funcval(r)
    if (!is.null(bound_prev)) {
      if (abs(bound_now - bound_prev) <= BOUND_STABILITY_RTOL * max(1, abs(bound_now))) {
        return(.finalize_linear_bound(quadratic, delta, omega, r, objective_vec, feas_tol))
      }
      if (abs(bound_now) >= UNBOUNDED_SCALING_FACTOR * max(abs(bound_prev), delta)) {
        return(list(bound = inf_bound, bounded = FALSE, valid = TRUE))
      }
    }
    bound_prev <- bound_now
  }
  # Functional interior-ish at the last box but neither stabilized nor clearly
  # scaled -> fail closed (conservative), as in the coordinate solver.
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
