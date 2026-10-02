# Internal helpers for evaluate_log_projection: candidate screening, result
# assembly, and Jacobian dispatch.

# Classify candidates before any transform: non-finite residuals are
# numerical failures; domain failures are established zero conditions only
log_projection_screen <- function(prep, b_mat, e, method) {
  bad <- colSums(!is.finite(e)) > 0L
  domain <- switch(method,
    log = colSums(e == 0) > 0L
  )
  list(bad = bad, domain = domain & !bad, e_mean = NULL)
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
    log = -2 * (prep$projection %*% (prep$w2 / e))
  )
}
