profile_linear_bound <- function(quadratic, objective, direction, evidence,
                                 evidence_index, control) {
  profile_assert_symmetric(quadratic, control)
  dimension <- ncol(quadratic$A_i[[1L]])
  assert_bad_argument_ok(
    length(objective) == dimension &&
      direction %in% c("min", "max") && length(control$solver_boxes) == 3L,
    "Invalid linear profile inputs"
  )
  invalid <- list(bound = NA_real_, bounded = FALSE, valid = FALSE)
  side <- if (direction == "min") "lower_state" else "upper_state"
  state <- evidence$summary[[side]][evidence_index]
  if (identical(state, "unbounded")) {
    return(list(bound = if (direction == "min") -Inf else Inf, bounded = FALSE, valid = TRUE))
  }
  if (!identical(state, "bounded")) {
    return(invalid)
  }
  if (all(objective == 0)) {
    return(list(bound = 0, bounded = TRUE, valid = TRUE))
  }
  normalized <- profile_objective_direction(objective)
  if (is.null(normalized)) {
    return(invalid)
  }
  profile_search_linear(quadratic, objective, normalized, direction, evidence, control)
}

profile_search_linear <- function(quadratic, objective, normalized, direction,
                                  evidence, control) {
  dimension <- ncol(quadratic$A_i[[1L]])
  delta <- profile_theta_scale(quadratic)
  omega <- profile_constraint_scales(quadratic, delta, control)
  sign_mult <- if (direction == "min") 1 else -1
  previous <- NULL
  for (box in control$solver_boxes) {
    bounds <- tryCatch(profile_scaled_bounds(delta, box, dimension),
      hetid_error_solver = function(error) NULL
    )
    if (is.null(bounds)) next
    result <- solve_quadratic_program(quadratic, rep(0, dimension),
      objective = function(theta) sign_mult * sum(normalized * theta),
      gradient = function(theta) sign_mult * normalized,
      lower = bounds$lower, upper = bounds$upper,
      objective_scale = "variable", control = control
    )
    candidate <- profile_bound_candidate(
      result, quadratic, evidence, objective,
      normalized, delta, omega, control
    )
    if (is.null(candidate)) next
    value <- candidate$value
    normalized_value <- candidate$normalized_value
    # accept a checked endpoint away from the box edge or once its value
    # stabilizes across boxes
    interior <- all(abs(result$phi) < control$bound_edge_rtol * box)
    stable <- !is.null(previous) && abs(normalized_value - previous) <=
      control$bound_stability_rtol * max(1, abs(normalized_value))
    if (interior || stable) {
      return(list(bound = value, bounded = TRUE, valid = TRUE, theta = candidate$theta))
    }
    previous <- normalized_value
  }
  list(bound = NA_real_, bounded = FALSE, valid = FALSE)
}

profile_bound_candidate <- function(result, quadratic, evidence, objective,
                                    normalized, delta, omega, control) {
  if (!all(is.finite(result$phi))) {
    return(NULL)
  }
  candidate <- profile_checked_candidate(evidence, delta * result$phi, control)
  if (is.null(candidate)) {
    return(NULL)
  }
  residual <- profile_residual(quadratic, candidate$theta, omega)
  if (!(is.finite(residual) && abs(residual) <= control$feasibility_tolerance)) {
    return(NULL)
  }
  value <- sum(objective * candidate$theta)
  normalized_value <- sum(normalized * candidate$theta)
  if (!is.finite(value) || !is.finite(normalized_value)) {
    return(NULL)
  }
  list(theta = candidate$theta, value = value, normalized_value = normalized_value)
}


profile_containing_bounds <- function(evidence, dimension) {
  out <- data.frame(
    outer_lower = rep(NA_real_, dimension),
    outer_upper = rep(NA_real_, dimension)
  )
  if (!is.null(evidence$boundedness)) {
    enclosure <- evidence$outer_bounds(diag(dimension), refine = TRUE)
    if (isTRUE(attr(enclosure, "empty")) && isTRUE(evidence$nonempty)) {
      stop_hetid(paste0(
        "Geometry conflict: a checked point is feasible but a ",
        "verified combination proves the set empty."
      ))
    }
    out$outer_lower <- enclosure$lower
    out$outer_upper <- enclosure$upper
  }
  states <- evidence$summary[seq_len(dimension), ]
  out$outer_lower[states$lower_state == "unbounded"] <- -Inf
  out$outer_upper[states$upper_state == "unbounded"] <- Inf
  out
}
