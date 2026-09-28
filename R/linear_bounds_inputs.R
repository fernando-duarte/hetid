# The fitted-system domain and argument axes used by the functional API
linear_bounds_frame <- function(fit, tau, n_grid, center, max_growth, search_limit) {
  validate_box_fit(fit)
  assert_scalar_finite(tau, "tau")
  assert_tau_values_ok(tau)
  assert_bad_argument_ok(tau > 0, "tau must be strictly positive", arg = "tau")
  assert_scalar_integer_in_range(n_grid, "n_grid", 3, .Machine$integer.max)
  assert_bad_argument_ok(n_grid %% 2L == 1L, "n_grid must be odd", arg = "n_grid")
  assert_scalar_integer_in_range(max_growth, "max_growth", 1, .Machine$integer.max)
  assert_scalar_finite(search_limit, "search_limit")
  assert_bad_argument_ok(search_limit >= 2, "search_limit must be at least two",
    arg = "search_limit"
  )
  n_components <- ncol(fit$w2)
  built <- build_quadratic_system(fit$gamma, rep(tau, n_components), fit$moments)
  if (!is.null(center)) {
    assert_bad_argument_ok(is.numeric(center) && is.null(dim(center)),
      "center must be a numeric vector",
      arg = "center"
    )
  }
  center <- resolve_box_center(fit, center, built$quadratic, n_components)
  if (!is.null(names(center))) {
    assert_bad_argument_ok(identical(names(center), colnames(fit$w2)),
      "center names must match the theta axis in order",
      arg = "center"
    )
  }
  basis <- identified_set_basis(built$components, center, built$quadratic)
  if (any(!is.finite(basis))) stop_hetid("Search frame exceeds the numeric range")
  names(center) <- rownames(basis) <- colnames(fit$w2)
  list(quadratic = built$quadratic, center = center, basis = basis)
}

validate_linear_objectives <- function(objectives, offsets, theta_names) {
  assert_bad_argument_ok(is.matrix(objectives) && is.numeric(objectives),
    "objectives must be a numeric matrix",
    arg = "objectives"
  )
  assert_dimension_ok(
    nrow(objectives) == length(theta_names) && ncol(objectives) > 0L,
    "objectives must have one row per theta and at least one column"
  )
  assert_numeric_finite_values(objectives, "objectives")
  assert_instrument_names(colnames(objectives), "objectives")
  if (!is.null(rownames(objectives))) {
    assert_bad_argument_ok(identical(rownames(objectives), theta_names),
      "objectives row names must match the theta axis in order",
      arg = "objectives"
    )
  }
  objective_names <- colnames(objectives)
  if (is.null(offsets)) offsets <- numeric(ncol(objectives))
  assert_bad_argument_ok(
    is.numeric(offsets) && is.null(dim(offsets)) &&
      length(offsets) == ncol(objectives) && all(is.finite(offsets)),
    "offsets must be a finite vector with one value per objective",
    arg = "offsets"
  )
  if (!is.null(names(offsets))) {
    assert_bad_argument_ok(identical(names(offsets), objective_names),
      "offset names must match objectives in order",
      arg = "offsets"
    )
  }
  stats::setNames(offsets, objective_names)
}

# Check finite witnesses with the existing checker and constraint-relative rounding scale
validate_linear_witnesses <- function(points, quadratic) {
  checker <- make_relative_feasibility_checker(quadratic)
  for (row in seq_len(nrow(points))) {
    if (!checker(points[row, ])) {
      stop_hetid("A finite bound witness fails the constraint-relative feasibility check")
    }
  }
  invisible(TRUE)
}

# Both proof kinds use the public theta axis, independent of moment-container labels
linear_bounds_evidence <- function(tails, objectives, theta_names) {
  tails <- lapply(tails, function(proof) {
    if (is.null(proof)) {
      return(NULL)
    }
    names(proof$direction) <- theta_names
    if (!is.null(proof$origin)) names(proof$origin) <- theta_names
    proof
  })
  stats::setNames(tails, colnames(objectives))
}
