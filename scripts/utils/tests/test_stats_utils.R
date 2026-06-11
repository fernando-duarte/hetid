# Tests for the stationarity-test p-value machinery in stats_utils.R.
# Run from the package root: Rscript scripts/utils/tests/test_stats_utils.R
source("scripts/utils/stats_utils.R")

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}

# Per-series seeds chosen so each realization is decisive for both tests
# (KPSS has limited power, so an arbitrary random-walk draw can stay near
# zero and fail to reject; seed 7 wanders enough to clamp at p = 0.01)
n <- 130
set.seed(42)
x_stationary <- as.numeric(arima.sim(list(ar = 0.5), n))
set.seed(7)
x_random_walk <- cumsum(rnorm(n))

# Known-series behavior: a stationary AR(0.5) rejects the ADF unit-root null
# decisively and never rejects the KPSS stationarity null; a random walk does
# the opposite
res_st <- perform_stationarity_tests(x_stationary, "stationary")
res_rw <- perform_stationarity_tests(x_random_walk, "random_walk")
check("stationary series: ADF p-value below 1e-6", res_st$ADF_pval < 1e-6)
check(
  "stationary series: KPSS p-value clamped at 0.10",
  isTRUE(all.equal(res_st$KPSS_pval, 0.10))
)
check("random walk: ADF p-value above 0.10", res_rw$ADF_pval > 0.10)
check(
  "random walk: KPSS p-value clamped at 0.01",
  isTRUE(all.equal(res_rw$KPSS_pval, 0.01))
)
adf_kpss_p <- c(
  res_st$ADF_pval, res_st$KPSS_pval, res_rw$ADF_pval, res_rw$KPSS_pval
)
check(
  "ADF and KPSS p-values lie in (0, 1)",
  all(adf_kpss_p > 0 & adf_kpss_p < 1)
)

# punitroot calibration: feeding urca's own critical values back through the
# MacKinnon (1996) response surface must recover the nominal levels
adf <- urca::ur.df(x_stationary, type = "drift", selectlags = "AIC")
p_adf_cv <- vapply(as.numeric(adf@cval[1, ]), function(cv) {
  urca::punitroot(cv, N = n, trend = "c", statistic = "t")
}, numeric(1))
check(
  "punitroot at ADF drift critical values is ~(0.01, 0.05, 0.10)",
  all(abs(p_adf_cv - c(0.01, 0.05, 0.10)) < 0.005)
)
pp <- urca::ur.pp(x_stationary, type = "Z-tau", model = "constant")
p_pp_cv <- vapply(as.numeric(pp@cval), function(cv) {
  urca::punitroot(cv, N = n, trend = "c", statistic = "t")
}, numeric(1))
check(
  "punitroot at PP critical values is ~(0.01, 0.05, 0.10)",
  all(abs(p_pp_cv - c(0.01, 0.05, 0.10)) < 0.005)
)
p_grid <- vapply(c(-4, -3, -2, -1), function(q) {
  urca::punitroot(q, N = n, trend = "c", statistic = "t")
}, numeric(1))
check("punitroot is monotone in the statistic", all(diff(p_grid) > 0))

# KPSS interpolation: exact at the table knots, monotone in between, clamped
# beyond the table, and consistent with the critical-value bands (the
# cross-check inside kpss_pvalue stops on any disagreement)
kpss_cval <- urca::ur.kpss(x_stationary, type = "mu")@cval
check(
  "ur.kpss critical values match KPSS (1992) Table 1",
  isTRUE(all.equal(as.numeric(kpss_cval), c(0.347, 0.463, 0.574, 0.739)))
)
p_knots <- vapply(as.numeric(kpss_cval), function(s) {
  kpss_pvalue(s, kpss_cval)
}, numeric(1))
check(
  "kpss_pvalue exact at the table knots",
  isTRUE(all.equal(p_knots, c(0.10, 0.05, 0.025, 0.01)))
)
stat_grid <- seq(0.05, 1.5, by = 0.01)
p_interp <- vapply(stat_grid, function(s) kpss_pvalue(s, kpss_cval), numeric(1))
check("kpss_pvalue monotone nonincreasing", all(diff(p_interp) <= 1e-12))
check(
  "kpss_pvalue clamped to [0.01, 0.10]",
  all(p_interp >= 0.01 & p_interp <= 0.10)
)
check(
  "critical-value band cross-check holds across the grid (no stop raised)",
  length(p_interp) == length(stat_grid)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
