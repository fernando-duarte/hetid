validate_variance_share_inputs <- function(prepared, control) {
  keys <- c("y", "x", "y2", "z")
  assert_bad_argument_ok(
    is.list(prepared) && all(keys %in% names(prepared)) &&
      !anyDuplicated(names(prepared)[names(prepared) %in% keys]),
    "prepared must contain exact named y, x, y2 and z elements",
    arg = "prepared"
  )
  y <- prepared[["y"]]
  assert_bad_argument_ok(quadratic_real_finite(y) && is.null(dim(y)) && length(y) > 0L,
    "prepared$y must be a nonempty finite real vector",
    arg = "prepared"
  )
  for (key in keys[-1L]) {
    block <- prepared[[key]]
    assert_bad_argument_ok(
      is.matrix(block) && quadratic_real_finite(block) &&
        ncol(block) >= 1L, paste0("prepared$", key, " must be a finite real matrix"),
      arg = "prepared"
    )
    assert_dimension_ok(nrow(block) == length(y), "Prepared row counts must match")
  }
  assert_dimension_ok(
    ncol(prepared[["z"]]) == 1L,
    "Variance shares require exactly one instrument"
  )
  for (key in c("x", "y2")) {
    assert_instrument_names(colnames(prepared[[key]]), paste0("prepared$", key))
  }
  assert_bad_argument_ok(
    !anyDuplicated(c(
      "(Intercept)",
      colnames(prepared[["x"]]), colnames(prepared[["y2"]])
    )),
    "Expected and news coefficient names must be distinct",
    arg = "prepared"
  )
  validate_variance_share_control(control)
  variance_share_grid_capacity(ncol(prepared[["y2"]]), control)
  invisible(prepared)
}

# Preserve the self-covariance arithmetic with a one-argument cross product
variance_share_self_cov <- function(mat) {
  mat <- as.matrix(mat)
  crossprod(sweep(mat, 2, colMeans(mat))) / nrow(mat)
}

variance_share_covariances <- function(y, x, y2, control) {
  s_e <- variance_share_self_cov(x)
  s_n <- variance_share_self_cov(y2)
  s_en <- centered_cov(x, y2)
  var_c <- drop(variance_share_self_cov(y))
  assert_bad_argument_ok(is.finite(var_c) && var_c > 0,
    "Outcome variance must be finite and positive",
    arg = "prepared"
  )
  for (s_block in list(s_e, s_n)) {
    assert_bad_argument_ok(all(is.finite(s_block)) && all(diag(s_block) > 0),
      "Block variances must be finite and positive",
      arg = "prepared"
    )
    correlation <- stats::cov2cor(s_block)
    assert_bad_argument_ok(
      all(abs(correlation[upper.tri(correlation)]) <= control$orthogonality_tolerance),
      "The columns within x and within y2 must be uncorrelated in sample.",
      arg = "prepared"
    )
  }
  assert_bad_argument_ok(all(is.finite(s_en)),
    "Cross-block covariances must be finite",
    arg = "prepared"
  )
  list(s_e = s_e, s_n = s_n, s_en = s_en, var_c = var_c)
}

variance_share_fit <- function(y, x, y2, z, control) {
  fit <- compute_tau0_system(y, y2, x, z,
    impose_null = FALSE,
    gamma = matrix(1, 1, ncol(y2)), tol = control$point_tolerance
  )
  assert_bad_argument_ok(
    identical(names(fit$beta1r), c("(Intercept)", colnames(x))) &&
      identical(rownames(fit$beta2r), colnames(y2)) &&
      identical(colnames(fit$beta2r), names(fit$beta1r)),
    "Mean coefficient axes must match the supplied block names",
    arg = "prepared"
  )
  if (is.null(fit$point) || !all(is.finite(fit$point$theta))) {
    stop_hetid("The mean system has no unique consistent tau-zero point.")
  }
  fit
}

variance_share_ols <- function(y, x, y2) {
  ols <- stats::lm.fit(cbind("(Intercept)" = 1, x, y2), y)$coefficients
  assert_bad_argument_ok(
    identical(names(ols), c("(Intercept)", colnames(x), colnames(y2))) &&
      all(is.finite(ols)), "OLS requires finite coefficients and a full-rank design",
    arg = "prepared"
  )
  ols
}
