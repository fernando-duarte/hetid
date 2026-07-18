# Canonical test-only log-variance data and engine-record constructors.

paper_test_variance_fixture <- function(
  seed = 20260713L,
  n = 120L,
  k = 2L,
  n_pc = 4L
) {
  set.seed(seed)
  w2 <- matrix(rnorm(n * k), n, k)
  pcr <- scale(
    matrix(rnorm(n * n_pc), n, n_pc),
    center = TRUE,
    scale = FALSE
  )
  colnames(pcr) <- paste0("l.pc", seq_len(n_pc))
  x_mat <- cbind(1, pcr)
  colnames(x_mat) <- c("(Intercept)", colnames(pcr))
  b_ref <- c(0.4, -0.25)
  log_h <- 0.3 +
    drop(pcr %*% c(0.5, -0.3, 0.2, -0.1))
  e_ref <- sqrt(exp(log_h)) * rnorm(n)
  e_ref[abs(e_ref) < 1e-3] <- 1e-3
  w1 <- drop(w2 %*% b_ref) + e_ref
  y <- drop(w1 - w2 %*% b_ref)^2
  b_zero <- b_ref
  w1_zero <- w1
  w1_zero[1L] <- drop(w2[1L, ] %*% b_zero)
  w1_zeros_multi <- w1
  w1_zeros_multi[1:3] <-
    drop(w2[1:3, , drop = FALSE] %*% b_zero)
  x_rankdef <- x_mat
  y_rankdef <- rep(0, n)
  positive <- 1:8
  x_rankdef[positive, ] <- cbind(
    1,
    outer(
      seq_along(positive) * 0.5,
      pcr[1L, ]
    )
  )
  y_rankdef[positive] <-
    seq_along(positive) * 0.2 + 0.5
  list(
    n = n,
    k = k,
    w2 = w2,
    pcr = pcr,
    x_mat = x_mat,
    b_ref = b_ref,
    e_ref = e_ref,
    w1 = w1,
    y = y,
    qtr = seq_len(n),
    b_zero = b_zero,
    w1_zero = w1_zero,
    w1_zeros_multi = w1_zeros_multi,
    x_rankdef = x_rankdef,
    y_rankdef = y_rankdef
  )
}

paper_test_fit_result <- function(
  coef = NULL,
  fit_status = "ok",
  converged = identical(fit_status, "ok"),
  objective = if (converged) 0 else NA_real_,
  score_norm = if (converged) 0 else NA_real_,
  convergence_code = if (converged) 0L else 1L,
  diagnostics = list(),
  warm_start = if (converged) coef else NULL
) {
  list(
    coef = coef,
    fit_status = fit_status,
    converged = converged,
    objective = objective,
    score_norm = score_norm,
    convergence_code = convergence_code,
    diagnostics = diagnostics,
    warm_start = warm_start
  )
}

paper_test_estimator_metadata <- function(
  estimator,
  target_functional,
  sample_id,
  spec_id,
  smoothness = "smooth",
  inner_solver = "closed form",
  response_scale = "identity",
  intercept_normalization = "none",
  cold_start_rtol = 1e-8,
  extra = list()
) {
  utils::modifyList(
    list(
      estimator = estimator,
      target_functional = target_functional,
      intercept_normalization = intercept_normalization,
      sample_id = sample_id,
      smoothness = smoothness,
      inner_solver = inner_solver,
      response_scale = response_scale,
      spec_id = spec_id,
      cold_start_rtol = cold_start_rtol
    ),
    extra
  )
}

paper_test_estimator <- function(
  metadata,
  fit_at_b,
  coef_labels = NULL,
  analyze_domain = NULL,
  coef_objective = NULL
) {
  estimator <- list(
    metadata = metadata,
    fit_at_b = fit_at_b
  )
  if (!is.null(coef_labels)) {
    estimator$coef_labels <- coef_labels
  }
  if (!is.null(analyze_domain)) {
    estimator$analyze_domain <- analyze_domain
  }
  if (!is.null(coef_objective)) {
    estimator$coef_objective <- coef_objective
  }
  estimator
}
