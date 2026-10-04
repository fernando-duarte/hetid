# The containing-box grid and its feasible rows depend on the set, not on the
# share, so one column builds them once for its three shares
variance_share_grid <- function(tab, quadratic, control) {
  domain <- profile_containing_box(tab)
  if (any(!is.finite(c(domain$lower, domain$upper)))) {
    return(NULL)
  }
  variance_share_grid_capacity(length(domain$lower), control)
  delta <- profile_theta_scale(quadratic)
  omega <- profile_constraint_scales(quadratic, delta, control)
  axes <- Map(
    function(l, h) seq(l, h, length.out = control$grid_points_per_axis),
    domain$lower, domain$upper
  )
  pts <- as.matrix(expand.grid(axes))
  feasible <- rowSums(profile_constraint_values(pts, quadratic, omega) >
    control$admission_tolerance) == 0
  pts <- pts[feasible, , drop = FALSE]
  if (!nrow(pts)) stop_hetid("No feasible grid point in the containing box.")
  list(domain = domain, pts = pts)
}

variance_share_range <- function(tab, quadratic, share, control,
                                 share_grid = variance_share_grid(tab, quadratic, control)) {
  if (is.null(share_grid)) {
    return(c(NA_real_, NA_real_))
  }
  domain <- share_grid$domain
  pts <- share_grid$pts
  vals <- share$value(pts)
  if (!all(is.finite(vals))) stop_hetid("Share values overflowed on the feasible grid.")
  starts <- pts[unique(c(
    which.min(vals), which.max(vals), apply(pts, 2, which.min),
    apply(pts, 2, which.max)
  )), , drop = FALSE]
  polished <- function(sign_mult) {
    candidates <- apply(starts, 1, function(x0) {
      result <- solve_quadratic_program(quadratic, x0,
        objective = function(theta) sign_mult * share$value(matrix(theta, 1)),
        gradient = function(theta) sign_mult * share$grad(theta),
        lower = domain$lower, upper = domain$upper, objective_scale = "none",
        control = control
      )
      if (!all(is.finite(result$theta))) {
        return(NA_real_)
      }
      # Keep the solver's pre-clamp residual and its one-sided feasibility gate
      theta <- pmin(pmax(result$theta, domain$lower), domain$upper)
      residual <- result$feasibility_residual
      if (is.finite(residual) && residual <= control$feasibility_tolerance) {
        share$value(matrix(theta, 1))
      } else {
        NA_real_
      }
    })
    candidates <- candidates[is.finite(candidates)]
    if (!length(candidates)) stop_hetid("No polished share extreme was accepted.")
    candidates
  }
  c(min(vals, polished(1)), max(vals, polished(-1)))
}
