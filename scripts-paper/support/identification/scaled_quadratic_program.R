# Generic scaled quadratic-program adapter.

solve_scaled_quadratic_program <- function(
  quadratic,
  x0,
  objective,
  gradient = NULL,
  lower,
  upper,
  method = c("slsqp", "cobyla"),
  objective_scale = c("none", "variable"),
  xtol_rel = PAPER_QUADRATIC_CONTROL$solver_xtol_rel,
  maxeval = PAPER_QUADRATIC_CONTROL$solver_maxeval,
  catch_errors = TRUE
) {
  method <- match.arg(method)
  objective_scale <- match.arg(objective_scale)
  stopifnot(
    is.function(objective),
    is.null(gradient) || is.function(gradient),
    length(x0) == length(lower),
    length(x0) == length(upper),
    all(is.finite(x0)),
    all(is.finite(lower)),
    all(is.finite(upper)),
    all(lower <= upper)
  )
  delta <- .derive_theta_scale(quadratic)
  omega <- .derive_constraint_scales(quadratic, delta)
  divisor <- if (objective_scale == "variable") delta else 1
  phi0 <- pmin(pmax(x0 / delta, lower / delta), upper / delta)
  objective_phi <- function(phi) objective(delta * phi) / divisor
  gradient_phi <- if (is.null(gradient)) {
    NULL
  } else {
    function(phi) delta * gradient(delta * phi) / divisor
  }
  constraints <- function(phi) {
    quadratic_constraint_values(delta * phi, quadratic, omega)
  }
  constraint_jacobian <- function(phi) {
    quadratic_constraint_jacobian(
      delta * phi,
      quadratic,
      omega,
      theta_scale = delta
    )
  }
  run <- function() {
    if (method == "slsqp") {
      stopifnot(is.function(gradient_phi))
      nloptr::slsqp(
        x0 = phi0,
        fn = objective_phi,
        gr = gradient_phi,
        lower = lower / delta,
        upper = upper / delta,
        hin = constraints,
        hinjac = constraint_jacobian,
        control = list(xtol_rel = xtol_rel, maxeval = maxeval),
        deprecatedBehavior = FALSE
      )
    } else {
      nloptr::cobyla(
        x0 = phi0,
        fn = objective_phi,
        lower = lower / delta,
        upper = upper / delta,
        hin = constraints,
        control = list(xtol_rel = xtol_rel, maxeval = maxeval),
        deprecatedBehavior = FALSE
      )
    }
  }
  result <- if (isTRUE(catch_errors)) {
    tryCatch(run(), error = function(error) NULL)
  } else {
    run()
  }
  if (is.null(result) || any(!is.finite(result$par))) {
    return(list(
      theta = rep(NA_real_, length(x0)),
      phi = rep(NA_real_, length(x0)),
      convergence = if (is.null(result)) NA_integer_ else result$convergence,
      feasibility_residual = NA_real_,
      delta = delta,
      omega = omega
    ))
  }
  theta <- delta * result$par
  list(
    theta = theta,
    phi = result$par,
    convergence = result$convergence,
    feasibility_residual = .feasibility_residual(quadratic, theta, omega),
    delta = delta,
    omega = omega
  )
}
