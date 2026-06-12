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

# NA fallback row matching the perform_all_hetero_tests() columns
suite_na_row <- function(var_name) {
  na_cols <- setNames(
    rep(list(NA_real_), 2 * length(suite_tests)),
    paste0(rep(suite_tests, each = 2), c("_stat", "_pval"))
  )
  data.frame(Variable = var_name, na_cols, stringsAsFactors = FALSE)
}
