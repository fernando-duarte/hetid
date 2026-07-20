# LM-style heteroskedasticity tests, the NA fallback row for the W2
# diagnostics battery, and the joint-relevance rank test. suite_na_row receives
# an explicit test vector or derives it from the canonical paper policy.

paper_source_once(paper_path("config", "diagnostics.R"))

# Breusch-Pagan LM: squared residuals on the PC levels, the direct check
# that Var(e2 | Z) moves with the instruments
bp_lm_test <- function(residuals, regressors) {
  aux_fit <- lm(residuals^2 ~ regressors)
  r_squared <- summary(aux_fit)$r.squared
  lm_stat <- length(residuals) * r_squared
  df <- ncol(regressors)
  list(statistic = lm_stat, df = df, p_value = 1 - pchisq(lm_stat, df), r_squared = r_squared)
}

# ARCH(1) LM: squared residuals on their one-period lag. Engle statistic
# n_aux * R^2, n_aux = auxiliary-regression rows (sum(valid))
arch1_test <- function(residuals) {
  res_sq <- residuals^2
  res_sq_lag <- c(NA, res_sq[-length(res_sq)])
  valid <- !is.na(res_sq) & !is.na(res_sq_lag)
  arch_fit <- lm(res_sq[valid] ~ res_sq_lag[valid])
  stat <- sum(valid) * summary(arch_fit)$r.squared
  list(statistic = stat, p_value = 1 - pchisq(stat, df = 1))
}

# Operational regime check for the diagnostics suite. In Regime A
# (span(1, Z) inside span(X), e.g. the default Z = first-stage PCs), OLS
# orthogonality makes the w2-on-Z refit's fitted values exactly zero, so
# fitted-value statistics (Anscombe) are 0/0-degenerate; with hook-supplied
# Z outside that span (Regime B) they carry genuine variation. The sd ratio
# is the empirical form of the span condition -- and conservative: a custom
# Z whose projection on w2 is negligible is treated as Regime A, where the
# fitted-value tests would be degenerate anyway. Threshold per the
# definedness gate in docs/heteroskedasticity_tests_general_instruments.tex.
w2_refit_fitted_ratio <- function(w2_resid, z_mat) {
  z_df <- as.data.frame(z_mat)
  max(vapply(seq_len(ncol(w2_resid)), function(k) {
    fit <- lm(w2_resid[, k] ~ ., data = z_df)
    stats::sd(fitted(fit)) / stats::sd(w2_resid[, k])
  }, numeric(1)))
}

# Regime-dependent suite selection for the diagnostics battery. Regime A
# excludes the 0/0-degenerate Anscombe; Regime B includes it. Cook-Weisberg
# is excluded in both regimes (duplicates the non-studentized Breusch-Pagan
# and over-rejects under heavy tails). The Goldfeld-Quandt deflator is fixed
# per regime, never chosen from the data at run time: column 2 in both
# regimes -- pc2 is the verified strongest variance driver of the default
# instruments, and the squared-PC calibration found the same (pc2_sq drives
# the variance, |t| = 5.0; pc1_sq carries no signal) -- with a fallback to
# column 1 for single-instrument hooks. Two-sided, because the skedastic
# default (time order, one-sided "greater") tests a rising variance time
# trend, not the Lewbel condition. See
# docs/reviews/hetero-test-investigation-2026-06-10.md and
# docs/heteroskedasticity_tests_general_instruments.tex.
select_diagnostics_suite <- function(
  w2_resid,
  z_mat,
  control = PAPER_HETEROSKEDASTICITY_CONTROL
) {
  stopifnot(
    ncol(z_mat) >= 1L,
    control$gq_deflator_position >= 1L,
    identical(names(control$suites), c("A", "B"))
  )
  ratio <- w2_refit_fitted_ratio(w2_resid, z_mat)
  regime <- if (ratio > control$fitted_sd_ratio_cutoff) "B" else "A"
  gq_position <- min(control$gq_deflator_position, ncol(z_mat))
  list(
    regime = regime,
    fitted_sd_ratio = ratio,
    suite_tests = control$suites[[regime]],
    gq_deflator = colnames(z_mat)[gq_position],
    gq_alternative = control$gq_alternative
  )
}

# KP-style joint-relevance rank test of a scalar driver z for the columns of
# y2: builds M_Z = (1/T) sum_t (z_t - zbar) y2_t y2_t' and tests the null
# rank(M_Z) = I - 1 (underidentification) against full rank I. With one rank
# deficiency the rk statistic is a Newey-West t-test that Cov(z, (u'y2)^2) = 0
# at the least-moved direction u (eigenvector of M_Z's smallest |eigenvalue|);
# chi-sq(1) under the null. One eigendecomposition also yields the spectrum
# summaries the joint-relevance table rows use: det, condition number,
# smallest singular value, and the separation ratio of the two smallest
# singular values (the chi-sq(1) reference needs the deficient direction
# unique, i.e. separation well above one).
rk_rank_test <- function(y2, z) {
  zc <- z - mean(z)
  m_z <- crossprod(y2, y2 * zc) / nrow(y2)
  es <- eigen(m_z, symmetric = TRUE)
  sv <- sort(abs(es$values), decreasing = TRUE)
  sv_min <- sv[[length(sv)]]
  u <- es$vectors[, which.min(abs(es$values))]
  h <- as.vector(y2 %*% u)^2
  s <- zc * (h - mean(h))
  n <- length(s)
  lag <- floor(4 * (n / 100)^(2 / 9))
  sc <- s - mean(s)
  gam <- vapply(0:lag, function(k) {
    sum(sc[seq_len(n - k)] * sc[seq_len(n - k) + k]) / n
  }, numeric(1))
  lrv <- gam[1] + 2 * sum((1 - seq_len(lag) / (lag + 1)) * gam[-1])
  stat <- n * mean(s)^2 / lrv
  list(
    m_z = m_z, det = prod(es$values), kappa = sv[[1L]] / sv_min,
    sv_min = sv_min, sep = sv[[length(sv) - 1L]] / sv_min,
    stat = stat, p = pchisq(stat, df = 1, lower.tail = FALSE), lag = lag
  )
}

# NA fallback row matching the perform_all_hetero_tests() columns
suite_na_row <- function(var_name, tests = paper_hetero_test_catalog()) {
  na_cols <- setNames(
    rep(list(NA_real_), 2 * length(tests)),
    paste0(rep(tests, each = 2), c("_stat", "_pval"))
  )
  data.frame(Variable = var_name, na_cols, stringsAsFactors = FALSE)
}
