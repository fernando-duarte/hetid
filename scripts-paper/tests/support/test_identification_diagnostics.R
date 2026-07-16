# Contract checks for the paper-owned identification diagnostics. Run from root:
# Rscript scripts-paper/tests/support/test_identification_diagnostics.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("support", "diagnostics", "identification_diagnostics.R"))

.pass <- 0L
.fail <- 0L
check <- function(label, condition) {
  if (isTRUE(condition)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}

# Scalar LM helpers preserve their documented auxiliary-regression formulas.
set.seed(19)
regressors <- cbind(z1 = rnorm(120), z2 = rnorm(120))
residuals <- exp(0.3 * regressors[, "z1"]) * rnorm(120)
bp <- bp_lm_test(residuals, regressors)
bp_fit <- lm(residuals^2 ~ regressors)
check("Breusch-Pagan degrees of freedom match regressor width", bp$df == 2L)
check(
  "Breusch-Pagan statistic matches the auxiliary regression",
  isTRUE(all.equal(bp$statistic, length(residuals) * summary(bp_fit)$r.squared))
)
check("Breusch-Pagan p-value is a probability", bp$p_value >= 0 && bp$p_value <= 1)

arch_residuals <- rnorm(120)
arch <- arch1_test(arch_residuals)
arch_sq <- arch_residuals^2
arch_fit <- lm(arch_sq[-1L] ~ arch_sq[-length(arch_sq)])
arch_expected <- (length(arch_residuals) - 2L) * summary(arch_fit)$r.squared
check(
  "ARCH statistic matches the lagged-square auxiliary regression",
  isTRUE(all.equal(arch$statistic, arch_expected))
)
check("ARCH p-value is a probability", arch$p_value >= 0 && arch$p_value <= 1)

suite_tests <- c("White", "BP")
na_row <- suite_na_row("maturity_1")
check("fallback row preserves its label", identical(na_row$Variable, "maturity_1"))
check(
  "fallback row follows the caller-selected suite schema",
  identical(
    names(na_row),
    c("Variable", "White_stat", "White_pval", "BP_stat", "BP_pval")
  ) && all(is.na(na_row[-1L]))
)

# Regime selection follows whether residuals retain a projection on the instruments.
set.seed(7)
z_mat <- matrix(rnorm(300), ncol = 3, dimnames = list(NULL, paste0("z", 1:3)))
w2_regime_a <- cbind(
  resid(lm(rnorm(100) ~ z_mat)),
  resid(lm(rnorm(100) ~ z_mat))
)
check(
  "orthogonal residuals have a negligible fitted-value ratio",
  w2_refit_fitted_ratio(w2_regime_a, z_mat) < 1e-10
)
w2_regime_b <- cbind(0.5 * z_mat[, 1L] + rnorm(100, sd = 0.5), rnorm(100))
check(
  "residuals projected on instruments clear the fitted-value gate",
  w2_refit_fitted_ratio(w2_regime_b, z_mat) > 1e-3
)
config_a <- select_diagnostics_suite(w2_regime_a, z_mat)
check(
  "orthogonal regime excludes Anscombe and aims GQ at the second instrument",
  config_a$regime == "A" && !"Anscombe" %in% config_a$suite_tests &&
    config_a$gq_deflator == colnames(z_mat)[2L]
)
config_b <- select_diagnostics_suite(w2_regime_b, z_mat)
check(
  "projected regime includes Anscombe and aims GQ at the second instrument",
  config_b$regime == "B" && "Anscombe" %in% config_b$suite_tests &&
    config_b$gq_deflator == colnames(z_mat)[2L]
)
z_one <- z_mat[, 1L, drop = FALSE]
w2_one <- cbind(0.5 * z_one[, 1L] + rnorm(100, sd = 0.5), rnorm(100))
check(
  "single-instrument suite aims GQ at the available instrument",
  select_diagnostics_suite(w2_one, z_one)$gq_deflator == colnames(z_one)[1L]
)

# The joint-rank diagnostic preserves its moment, spectrum, and scaling contracts.
set.seed(42)
n <- 240
z <- rnorm(n)
volatility <- exp(0.25 * z)
y2_full <- cbind(
  volatility * rnorm(n),
  volatility * rnorm(n),
  volatility * rnorm(n)
)
rk <- rk_rank_test(y2_full, z)
z_centered <- z - mean(z)
check(
  "joint-rank diagonal equals the centered covariance moment",
  all(abs(diag(rk$m_z) - colMeans(z_centered * y2_full^2)) < 1e-12)
)
check("joint-rank moment matrix is symmetric", all(abs(rk$m_z - t(rk$m_z)) < 1e-14))
check("reported determinant matches the moment matrix", isTRUE(all.equal(rk$det, det(rk$m_z))))
check(
  "condition and separation ratios follow the sorted spectrum",
  rk$kappa >= rk$sep && rk$sep >= 1 && rk$sv_min > 0
)
check(
  "smallest singular value matches a direct decomposition",
  isTRUE(all.equal(rk$sv_min, min(svd(rk$m_z)$d)))
)
check("joint-rank p-value is a probability", rk$p > 0 && rk$p < 1)
expected_lag <- floor(4 * (n / 100)^(2 / 9))
check("Newey-West lag follows the automatic rule", rk$lag == expected_lag)
rk_scaled <- rk_rank_test(2 * y2_full, 3 * z)
check(
  "joint-rank statistic is invariant to input rescaling",
  isTRUE(all.equal(rk_scaled$stat, rk$stat))
)
check("full-rank alternative rejects", rk$p < 0.05)
y2_deficient <- cbind(volatility * rnorm(n), volatility * rnorm(n), rnorm(n))
rk_deficient <- rk_rank_test(y2_deficient, z)
check("rank-deficient null is not rejected", rk_deficient$p > 0.05)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
