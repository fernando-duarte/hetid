# Internal helpers for evaluate_log_projection and compute_log_projection_vcov:
# argument checks, candidate screening and passes, result assembly, and
# Jacobian dispatch.

# Validates prep, method, multiplier and the candidate(s); returns the
# candidate matrix and whether one vector was supplied
log_projection_args <- function(prep, b, method, multiplier) {
  assert_hetid_log_projection_prep(prep)
  method_values <- LOG_PROJECTION_CONTROL$METHODS
  assert_bad_argument_ok(
    is.character(method) && length(method) == 1L && !is.na(method) &&
      method %in% method_values,
    paste0("method must be one of: ", paste(method_values, collapse = ", ")),
    arg = "method"
  )
  assert_scalar_finite(multiplier, "multiplier")
  assert_bad_argument_ok(multiplier > 0, "multiplier must be positive",
    arg = "multiplier"
  )
  is_single <- is.null(dim(b))
  b_mat <- if (is_single) matrix(b, nrow = 1L) else b
  assert_bad_argument_ok(
    is.numeric(b_mat) && is.matrix(b_mat) && nrow(b_mat) >= 1L,
    "b must be a numeric vector or a matrix with one candidate per row",
    arg = "b"
  )
  assert_numeric_finite_values(b_mat, "b")
  assert_dimension_ok(
    ncol(b_mat) == ncol(prep$w2),
    sprintf("each candidate needs %d entries, not %d", ncol(prep$w2), ncol(b_mat))
  )
  # a permuted named candidate would silently change the residuals
  b_names <- if (is_single) names(b) else colnames(b)
  assert_bad_argument_ok(
    is.null(b_names) || is.null(colnames(prep$w2)) ||
      identical(b_names, colnames(prep$w2)),
    "names of b must equal colnames(w2) in order",
    arg = "b"
  )
  list(b_mat = b_mat, is_single = is_single)
}

# Screens the candidates and runs the method's passes on the live columns
log_projection_passes <- function(prep, b_mat, method, multiplier) {
  e <- prep$w1 - prep$w2 %*% t(b_mat)
  screening <- log_projection_screen(prep, b_mat, e, method)
  run <- !screening$bad & (method == "log" | !screening$domain)
  pass <- NULL
  if (any(run)) {
    e_run <- e[, run, drop = FALSE]
    log_x <- 2 * log(abs(e_run))
    pass <- switch(method,
      log = log_projection_log(prep, e_run, log_x),
      log_plus = log_projection_plus(prep, e_run, log_x, multiplier),
      log_fuller = log_projection_fuller(
        prep, screening$e_mean[, run, drop = FALSE], e_run, log_x, multiplier
      )
    )
    pass$e <- e_run
    pass$log_x <- log_x
  }
  list(screening = screening, run = run, pass = pass)
}

# Classify candidates before any transform: non-finite residuals are
# numerical failures; domain failures are established zero conditions only
log_projection_screen <- function(prep, b_mat, e, method) {
  bad <- colSums(!is.finite(e)) > 0L
  e_mean <- NULL
  domain <- switch(method,
    log = colSums(e == 0) > 0L,
    log_plus = rep(!is.finite(prep$log_scale_common), ncol(e)),
    log_fuller = {
      e_mean <- prep$w1_mean - prep$w2_mean %*% t(b_mat)
      bad <- bad | colSums(!is.finite(e_mean)) > 0L
      colSums(e_mean != 0) == 0L
    }
  )
  list(bad = bad, domain = domain & !bad, e_mean = e_mean)
}

# Spread run-column values back to all k candidates, NA elsewhere
log_projection_fill <- function(values, run) {
  if (is.matrix(values)) {
    out <- matrix(NA_real_, nrow(values), length(run),
      dimnames = list(rownames(values), NULL)
    )
    out[, run] <- values
    return(out)
  }
  out <- rep(NA_real_, length(run))
  out[run] <- values
  out
}

log_projection_result <- function(prep, method, multiplier, is_single,
                                  jacobian, screening, run, pass) {
  st <- LOG_PROJECTION_STATUS
  coef_mat <- matrix(NA_real_, nrow(prep$projection), length(run))
  diagnostics <- list()
  if (!is.null(pass)) {
    coef_mat[, run] <- pass$coef
    diagnostics <- lapply(pass$diagnostics, log_projection_fill, run = run)
  }
  rownames(coef_mat) <- rownames(prep$projection)
  finite <- apply(is.finite(coef_mat), 2L, all)
  status <- ifelse(screening$bad, st[["numerical_failure"]],
    ifelse(screening$domain, st[["domain_failure"]],
      ifelse(finite, st[["ok"]], st[["numerical_failure"]])
    )
  )
  status <- unname(status)
  jac <- NULL
  if (is_single && jacobian && status == st[["ok"]]) {
    jac <- log_projection_jacobian(prep, method, pass)
    dimnames(jac) <- list(rownames(prep$projection), colnames(prep$w2))
    if (!all(is.finite(jac))) {
      status <- st[["numerical_failure"]]
      jac <- NULL
    }
  }
  # cleared only once the status is final; log keeps its divergent
  # coefficients at a zero residual (the log-OLS divergence semantics)
  clear <- status == st[["numerical_failure"]] |
    (method != "log" & status == st[["domain_failure"]])
  coef_mat[, clear] <- NA_real_
  diagnostics <- c(diagnostics, list(
    method = method, multiplier = multiplier,
    n_mean = attr(prep, "n_mean"), n_vol = attr(prep, "n_vol")
  ))
  if (is_single) {
    coef_mat <- coef_mat[, 1L]
    diagnostics <- lapply(diagnostics, function(v) {
      if (is.matrix(v)) v[, 1L] else v
    })
  }
  list(
    coef = coef_mat, jacobian = jac, status = status,
    diagnostics = diagnostics
  )
}

log_projection_jacobian <- function(prep, method, pass) {
  e <- drop(pass$e)
  switch(method,
    log = -2 * (prep$projection %*% (prep$w2 / e)),
    log_plus = {
      d <- log_projection_resid_deriv(e, drop(pass$log_x) / 2, drop(pass$work$a))
      -(prep$projection %*% (d * prep$w2))
    },
    log_fuller = log_projection_fuller_jacobian(
      prep, e, drop(pass$log_x) / 2, pass$work
    )
  )
}
