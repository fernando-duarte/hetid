# Foundation math for the joint-null theta_R = 0 distance diagnostic: the owned
# projection wrapper, the frozen slope scales, the zero-safe log-squared response
# primitive, the vectorized/scalar scaled-distance objective, and its analytic
# gradient. Every function takes qtr-aligned matrices; the driver owns the joins,
# prints, and writes. Consumes logvar_projection / logvar_theta_jacobian from
# log_var_eq_map.R. Definitions only; sourced by log_var_eq.R after the map and by
# scripts/utils/tests/test_logvar_joint_null_helpers.R. No clamping, no epsilon
# inside a log, no residual floor.

# provenance registry so the driver's single projection is owned once per
# sample_id: a later call presenting a different qtr-aligned design under the same
# id is a misalignment and fails closed rather than silently returning a permuted
# projection matrix
.logvar_joint_null_proj_registry <- new.env(parent = emptyenv())

# owned projection P = (R'R)^{-1} R', R = (1, PC_R): calls logvar_projection once,
# asserts shape/finiteness/column identity and qtr length, and records the design
# under sample_id so a mismatched later design under the same id stops
logvar_joint_null_projection <- function(pcr, qtr, sample_id) {
  pcr <- as.matrix(pcr)
  expected <- paste0("l.pc", seq_len(ncol(pcr)))
  if (is.null(colnames(pcr)) || !identical(colnames(pcr), expected)) {
    stop("pcr columns must be named l.pc1..l.pcK for the joint-null projection")
  }
  if (length(qtr) != nrow(pcr)) {
    stop("qtr length must equal nrow(pcr) for the joint-null projection")
  }
  proj <- logvar_projection(pcr)
  if (!identical(dim(proj), c(ncol(pcr) + 1L, nrow(pcr))) ||
    any(!is.finite(proj))) {
    stop("joint-null projection must be a finite (ncol(pcr)+1) x nrow(pcr) matrix")
  }
  key <- as.character(sample_id)
  prior <- .logvar_joint_null_proj_registry[[key]]
  if (!is.null(prior)) {
    if (!identical(prior$pcr, pcr) || !identical(prior$qtr, qtr)) {
      stop("misaligned design: sample_id already owns a different qtr-aligned pcr")
    }
    return(prior$proj)
  }
  .logvar_joint_null_proj_registry[[key]] <- list(pcr = pcr, qtr = qtr, proj = proj)
  proj
}

# frozen slope scales d_j = 1 / sd(PC_{R,j}) and d_inv2 = d^-2, derived once from
# the predeclared de-meaned design; strictly positive and finite or a hard stop
logvar_joint_null_scales <- function(pcr) {
  pcr <- as.matrix(pcr)
  expected <- paste0("l.pc", seq_len(ncol(pcr)))
  if (is.null(colnames(pcr)) || !identical(colnames(pcr), expected)) {
    stop("pcr columns must be named l.pc1..l.pcK for the joint-null scales")
  }
  d <- 1 / apply(pcr, 2L, stats::sd)
  if (any(!is.finite(d)) || any(d <= 0)) {
    stop("joint-null scales d = 1 / sd(pcr) must be finite and strictly positive")
  }
  list(d = d, d_inv2 = d^-2)
}

# zero-safe log-squared response: mark a column unavailable when ANY residual is
# exactly zero (before any log), then log_sq = 2 log|e| for the available columns
# (the n x 0 case is valid); never square, floor, or drop a residual
logvar_joint_null_response <- function(eps) {
  if (is.null(dim(eps))) eps <- matrix(eps, ncol = 1L)
  eps <- as.matrix(eps)
  if (!is.numeric(eps)) {
    stop("residuals must be numeric for the joint-null response")
  }
  unavailable <- vapply(
    seq_len(ncol(eps)), function(j) any(eps[, j] == 0), logical(1L)
  )
  available_ids <- which(!unavailable)
  log_sq <- if (length(available_ids)) {
    2 * log(abs(eps[, available_ids, drop = FALSE]))
  } else {
    matrix(numeric(0L), nrow = nrow(eps), ncol = 0L)
  }
  list(unavailable = unavailable, available_ids = available_ids, log_sq = log_sq)
}

# scaled squared-distance objective q(b) = 0.5 ||D^{-1} s(b)||^2 with the pinned
# algebra, vectorized over rows of b (grid) or a length-K vector (scalar -> one
# column); returns q, the slope vector s, the full coefficient theta, and the
# domain mask, re-expanded to every b row with typed NA for unavailable columns
logvar_joint_null_objective <- function(b, w1, w2, proj, d_inv2) {
  b_mat <- if (is.null(dim(b))) matrix(b, nrow = 1L) else as.matrix(b)
  stopifnot(ncol(b_mat) == ncol(w2), length(d_inv2) == nrow(proj) - 1L)
  nb <- nrow(b_mat)
  n_slope <- length(d_inv2)
  sqrt_dinv2 <- sqrt(d_inv2)
  q <- rep(NA_real_, nb)
  s <- matrix(NA_real_, n_slope, nb)
  theta <- matrix(NA_real_, nrow(proj), nb)
  unavailable <- rep(TRUE, nb)
  chunk <- 5000L
  for (from in seq(1L, nb, by = chunk)) {
    cols <- from:min(from + chunk - 1L, nb)
    eps <- w1 - w2 %*% t(b_mat[cols, , drop = FALSE])
    response <- logvar_joint_null_response(eps)
    unavailable[cols] <- response$unavailable
    avail <- response$available_ids
    if (length(avail)) {
      th <- proj %*% response$log_sq
      slopes <- th[-1L, , drop = FALSE]
      scaled <- sweep(slopes, 1L, sqrt_dinv2, "*")
      hit <- cols[avail]
      q[hit] <- 0.5 * colSums(scaled^2)
      s[, hit] <- slopes
      theta[, hit] <- th
    }
  }
  if (nb == 1L) {
    list(q = q[1L], s = s[, 1L], theta = theta[, 1L], unavailable = unavailable[1L])
  } else {
    list(q = q, s = s, theta = theta, unavailable = unavailable)
  }
}

# analytic gradient grad q = J_s' D^{-2} s with J_s = A J_theta from the map
# Jacobian; the slope vector reuses the same zero-safe response as the objective
# so the finite-difference check is exact. A crossing or nonfinite grad rejects.
logvar_joint_null_gradient <- function(b, w1, w2, proj, d_inv2) {
  b <- as.numeric(b)
  stopifnot(length(b) == ncol(w2), length(d_inv2) == nrow(proj) - 1L)
  eps <- drop(w1 - w2 %*% b)
  if (any(eps == 0)) {
    stop("joint-null gradient is undefined at an exact residual zero")
  }
  response <- logvar_joint_null_response(eps)
  s <- drop(proj %*% response$log_sq)[-1L]
  j_s <- logvar_theta_jacobian(b, w1, w2, proj)[-1L, , drop = FALSE]
  grad <- drop(crossprod(j_s, d_inv2 * s))
  if (any(!is.finite(grad))) {
    stop("joint-null gradient is nonfinite")
  }
  grad
}
