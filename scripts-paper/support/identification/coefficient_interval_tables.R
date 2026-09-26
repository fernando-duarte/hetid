paper_source_once(paper_path("support", "identification", "containing_box.R"))

# Coordinate and structural objectives share evidence and checked points.
coef_interval_tables_from_quadratic <- function(qs, beta1r, beta2r,
                                                points = NULL, evidence = NULL) {
  dimension <- nrow(beta2r)
  objectives <- cbind(diag(dimension), beta2r)
  if (is.null(evidence)) evidence <- paper_profile_evidence(qs, objectives, points)
  tb <- solve_all_profile_bounds(qs, evidence = evidence)
  accepted <- attr(tb, "profile_points")
  theta_lower <- paper_endpoint_status_from_flags(tb$bounded_lower, tb$valid_lower)
  theta_upper <- paper_endpoint_status_from_flags(tb$bounded_upper, tb$valid_upper)
  theta <- data.frame(
    coef = rownames(beta2r), set_lower = tb$lower, set_upper = tb$upper,
    status = paper_endpoint_status_reduce(theta_lower, theta_upper),
    lower_status = theta_lower, upper_status = theta_upper,
    row.names = NULL, stringsAsFactors = FALSE
  )
  theta <- cbind(theta, profile_containing_bounds(evidence, dimension))
  beta_rows <- lapply(seq_along(beta1r), function(j) {
    loading <- beta2r[, j]
    fmin <- solve_linear_functional_bound(qs, loading, "min",
      evidence = evidence, evidence_index = dimension + j
    )
    fmax <- solve_linear_functional_bound(qs, loading, "max",
      evidence = evidence, evidence_index = dimension + j
    )
    lower <- paper_endpoint_status_from_flags(fmax$bounded, fmax$valid)
    upper <- paper_endpoint_status_from_flags(fmin$bounded, fmin$valid)
    tab <- data.frame(
      coef = names(beta1r)[j],
      set_lower = unname(beta1r[j]) - fmax$bound,
      set_upper = unname(beta1r[j]) - fmin$bound,
      status = paper_endpoint_status_reduce(lower, upper),
      lower_status = lower, upper_status = upper,
      row.names = NULL, stringsAsFactors = FALSE
    )
    list(
      tab = tab, points = Filter(Negate(is.null), list(fmin$theta, fmax$theta)),
      corrections = rbind(
        profile_correction_record(fmin, dimension + j, "min"),
        profile_correction_record(fmax, dimension + j, "max")
      )
    )
  })
  out <- list(beta1 = do.call(rbind, lapply(beta_rows, `[[`, "tab")), theta = theta)
  attr(out, "profile_points") <- c(
    accepted,
    unlist(lapply(beta_rows, `[[`, "points"), recursive = FALSE)
  )
  attr(out, "profile_corrections") <- rbind(
    attr(tb, "profile_corrections"),
    do.call(rbind, lapply(beta_rows, `[[`, "corrections"))
  )
  out
}

# The fixed-gamma builder also supplies a tau-zero nonemptiness candidate.
mean_profile_system <- function(gamma, tau, moments) {
  built <- build_pipeline_quadratic_system(gamma, rep(tau, ncol(gamma)), moments)
  point <- hetid::compute_tau0_point(built$components,
    tol = PAPER_QUADRATIC_CONTROL$point_identification_tolerance
  )
  points <- if (is.null(point)) NULL else matrix(point$theta, nrow = 1L)
  list(quadratic = built$quadratic, points = points)
}

coef_interval_tables <- function(gamma, tau, moments, beta1r, beta2r) {
  stopifnot(nrow(beta2r) == ncol(gamma), ncol(beta2r) == length(beta1r))
  stopifnot(identical(colnames(beta2r), names(beta1r)))
  system <- mean_profile_system(gamma, tau, moments)
  coef_interval_tables_from_quadratic(
    system$quadratic, beta1r, beta2r,
    points = system$points
  )
}
