validate_profile_control <- function(control) {
  keys <- names(QUADRATIC_PROFILE_CONTROL)
  assert_bad_argument_ok(is.list(control) && !anyDuplicated(names(control)) &&
    all(keys %in% names(control)), "control lacks quadratic profile settings", arg = "control")
  scalar <- setdiff(keys, "solver_boxes")
  for (key in scalar) assert_scalar_finite(control[[key]], paste0("control$", key))
  nonnegative <- c(
    "symmetry_rtol", "feasibility_tolerance", "admission_tolerance",
    "candidate_correction_rtol", "bound_stability_rtol"
  )
  assert_bad_argument_ok(all(vapply(control[nonnegative], function(x) x >= 0, logical(1))),
    "Profile tolerances must be nonnegative",
    arg = "control"
  )
  assert_bad_argument_ok(control$constraint_scale_floor_rtol > 0 &&
    control$solver_xtol_rel > 0 && control$bound_edge_rtol > 0 &&
    control$bound_edge_rtol < 1, "Invalid positive profile controls", arg = "control")
  for (key in c("solver_maxeval", "multistart_rounds", "multistart_dedup_digits")) {
    assert_scalar_integer_in_range(
      control[[key]], paste0("control$", key),
      1, .Machine$integer.max
    )
  }
  boxes <- control$solver_boxes
  assert_bad_argument_ok(
    quadratic_real_finite(boxes) && is.null(dim(boxes)) &&
      length(boxes) == 3L && all(boxes > 0),
    "solver_boxes must contain three positive finite values",
    arg = "control"
  )
  invisible(control)
}

validate_profile_betas <- function(beta1r, beta2r, dimension) {
  assert_bad_argument_ok(quadratic_real_finite(beta1r) && is.null(dim(beta1r)) &&
    length(beta1r) > 0L, "beta1r must be a nonempty finite vector", arg = "beta1r")
  assert_bad_argument_ok(is.matrix(beta2r) && quadratic_real_finite(beta2r),
    "beta2r must be a finite matrix",
    arg = "beta2r"
  )
  assert_dimension_ok(
    nrow(beta2r) == dimension && ncol(beta2r) == length(beta1r),
    "beta2r dimensions must match theta and beta1r"
  )
  assert_instrument_names(names(beta1r), "beta1r")
  assert_instrument_names(rownames(beta2r), "beta2r")
  assert_bad_argument_ok(identical(colnames(beta2r), names(beta1r)),
    "beta2r column names must match beta1r names in order",
    arg = "beta2r"
  )
  invisible(TRUE)
}

validate_profile_taus <- function(taus, arg, allow_empty = FALSE) {
  assert_bad_argument_ok(
    quadratic_real_finite(taus) && is.null(dim(taus)) &&
      (allow_empty || length(taus) > 0L) && all(taus > 0 & taus < 1),
    paste0(arg, " must contain finite slacks in (0, 1)"),
    arg = arg
  )
  invisible(taus)
}

validate_profile_fit <- function(fit) {
  validate_box_fit(fit)
  assert_bad_argument_ok(!is.null(fit$point),
    "fit must carry a unique consistent tau-zero point",
    arg = "fit"
  )
  components <- compute_identified_set_components(fit$gamma, fit$moments)
  tolerance <- attr(fit, "tol")
  current <- compute_tau0_point(components, tol = tolerance)
  assert_bad_argument_ok(!is.null(current),
    "fit must have a unique consistent point for its current active system",
    arg = "fit"
  )
  distance <- max(abs(fit$point$theta - current$theta))
  assert_bad_argument_ok(
    is.finite(distance) &&
      distance <= tolerance * max(1, max(abs(current$theta))),
    "fit carries a stale tau-zero point for its current active system",
    arg = "fit"
  )
  invisible(fit)
}
