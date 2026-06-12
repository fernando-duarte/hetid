# LM-style heteroskedasticity tests and the NA fallback row for the W2
# diagnostics battery. Moved verbatim from heteroskedasticity_tests.R
# (stage 02), its only consumer; suite_na_row resolves that script's
# suite_tests vector at call time.

# Breusch-Pagan LM: squared residuals on the PC levels, the direct check
# that Var(e2 | Z) moves with the instruments
bp_lm_test <- function(residuals, regressors) {
  aux_fit <- lm(residuals^2 ~ regressors)
  r_squared <- summary(aux_fit)$r.squared
  lm_stat <- length(residuals) * r_squared
  df <- ncol(regressors)
  list(statistic = lm_stat, df = df, p_value = 1 - pchisq(lm_stat, df), r_squared = r_squared)
}

# ARCH(1) LM: squared residuals on their one-period lag
arch1_test <- function(residuals) {
  res_sq <- residuals^2
  res_sq_lag <- c(NA, res_sq[-length(res_sq)])
  valid <- !is.na(res_sq) & !is.na(res_sq_lag)
  arch_fit <- lm(res_sq[valid] ~ res_sq_lag[valid])
  stat <- (sum(valid) - 1) * summary(arch_fit)$r.squared
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
select_diagnostics_suite <- function(w2_resid, z_mat) {
  ratio <- w2_refit_fitted_ratio(w2_resid, z_mat)
  regime <- if (ratio > 1e-3) "B" else "A"
  suite <- if (regime == "B") {
    c("White", "BP", "GQ", "Harvey", "Anscombe")
  } else {
    c("White", "BP", "GQ", "Harvey")
  }
  list(
    regime = regime, fitted_sd_ratio = ratio, suite_tests = suite,
    gq_deflator = colnames(z_mat)[min(2L, ncol(z_mat))],
    gq_alternative = "two.sided"
  )
}

# NA fallback row matching the perform_all_hetero_tests() columns
suite_na_row <- function(var_name) {
  na_cols <- setNames(
    rep(list(NA_real_), 2 * length(suite_tests)),
    paste0(rep(suite_tests, each = 2), c("_stat", "_pval"))
  )
  data.frame(Variable = var_name, na_cols, stringsAsFactors = FALSE)
}
