#' Scaled SLSQP Search over Quadratic Constraints
#'
#' Supplies theta = delta * phi and positive per-constraint scaling to SLSQP.
#' A returned finite vector is an optimizer candidate, not a feasibility,
#' boundedness, convergence, or global-optimality certificate.
#'
#' @param quadratic Nonempty finite symmetric system with A_i, b_i and c_i.
#' @param x0 Finite starting vector. It is clipped to the search box after scaling.
#' @param objective Function returning one numeric objective at theta.
#' @param gradient Function returning its gradient in theta order.
#' @param lower,upper Finite vectors defining the search box in theta units.
#' @param objective_scale Either none, or variable to divide the objective by delta.
#' @param control List containing the QUADRATIC_PROFILE_CONTROL settings.
#' @param catch_errors Logical; return missing candidate fields on numerical errors
#'   when TRUE. Otherwise preserve structured callback conditions and wrap other
#'   errors in hetid_error_solver. Argument and missing-backend errors always stop.
#' @return A list with theta, phi and feasibility_residual. The last is the
#'   largest normalized constraint value. Failed or nonfinite candidates have
#'   missing vectors and residual. Solver termination alone does not accept an endpoint.
#' @export
#' @examples
#' qs <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
#' solve_quadratic_program(
#'   qs, c(0, 0), function(x) x[1], function(x) c(1, 0),
#'   c(-2, -2), c(2, 2)
#' )
solve_quadratic_program <- function(quadratic, x0, objective, gradient, lower, upper,
                                    objective_scale = c("none", "variable"),
                                    control = QUADRATIC_PROFILE_CONTROL,
                                    catch_errors = TRUE) {
  objective_scale <- tryCatch(match.arg(objective_scale),
    error = function(error) stop_bad_argument(conditionMessage(error), arg = "objective_scale")
  )
  dimension <- quadratic_validate_system(quadratic)
  validate_profile_control(control)
  profile_solve_checked(
    quadratic, dimension, x0, objective, gradient, lower, upper,
    objective_scale, control, catch_errors
  )
}

# Solver entry for package callers whose system and controls are already
# validated; they may also pass the scales they hold, so neither the system
# check nor the eigen decompositions repeat on every solve
profile_solve_checked <- function(quadratic, dimension, x0, objective, gradient, lower,
                                  upper, objective_scale, control, catch_errors = TRUE,
                                  delta = NULL, omega = NULL) {
  assert_flag(catch_errors, "catch_errors")
  assert_bad_argument_ok(
    is.function(objective) && is.function(gradient),
    "objective and gradient must be functions"
  )
  for (name in c("x0", "lower", "upper")) {
    value <- get(name)
    assert_bad_argument_ok(quadratic_real_finite(value) && is.null(dim(value)),
      paste0(name, " must be a finite numeric vector"),
      arg = name
    )
    assert_dimension_ok(length(value) == dimension, paste0(name, " has wrong dimension"))
  }
  assert_bad_argument_ok(all(lower <= upper), "lower must not exceed upper")
  tryCatch(profile_solve(
    quadratic, x0, objective, gradient, lower, upper,
    objective_scale, control, delta, omega
  ), error = function(error) {
    if (catch_errors) {
      return(profile_missing_candidate(dimension))
    }
    profile_numeric_error(error)
  })
}

profile_missing_candidate <- function(dimension) {
  list(
    theta = rep(NA_real_, dimension), phi = rep(NA_real_, dimension),
    feasibility_residual = NA_real_
  )
}

profile_solve <- function(quadratic, x0, objective, gradient, lower, upper,
                          objective_scale, control, delta = NULL, omega = NULL) {
  if (is.null(delta)) delta <- profile_theta_scale(quadratic)
  if (is.null(omega)) omega <- profile_constraint_scales(quadratic, delta, control)
  if (!is.finite(delta) || delta <= 0 || any(!is.finite(omega)) || any(omega <= 0)) {
    stop(new_hetid_error(
      "Quadratic profile scaling exceeds the numeric range",
      "hetid_error_solver"
    ))
  }
  divisor <- if (objective_scale == "variable") delta else 1
  phi0 <- pmin(pmax(x0 / delta, lower / delta), upper / delta)
  result <- nloptr::slsqp(
    x0 = phi0,
    fn = function(phi) objective(delta * phi) / divisor,
    gr = function(phi) delta * gradient(delta * phi) / divisor,
    lower = lower / delta,
    upper = upper / delta,
    hin = function(phi) profile_constraint_values(delta * phi, quadratic, omega),
    hinjac = function(phi) {
      profile_constraint_jacobian(delta * phi, quadratic, omega, theta_scale = delta)
    },
    control = list(xtol_rel = control$solver_xtol_rel, maxeval = control$solver_maxeval),
    deprecatedBehavior = FALSE
  )
  if (any(!is.finite(result$par))) {
    return(profile_missing_candidate(length(x0)))
  }
  theta <- delta * result$par
  if (any(!is.finite(theta))) {
    return(profile_missing_candidate(length(x0)))
  }
  list(
    theta = theta, phi = result$par,
    feasibility_residual = profile_residual(quadratic, theta, omega)
  )
}
