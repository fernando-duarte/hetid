# Contract checks for the paper-owned statistical support modules. Run from root:
# Rscript scripts-paper/tests/support/test_statistics.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check
paper_source_once(paper_path("support", "statistics", "api.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "latex", "table_environment.R"))
paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("log_variance", "tables", "panel_block.R"))
paper_source_once(paper_path("log_variance", "tables", "console_formatting.R"))
paper_source_once(paper_path("tests", "support", "reporting_checks.R"))
paper_source_once(paper_path("tests", "support", "cell_formatting_checks.R"))

# Moving blocks retain their requested length, range, and within-block order.
set.seed(91)
idx <- mbb_index(11L, 4L)
check("moving-block draw has the requested length", length(idx) == 11L)
check("moving-block draw stays inside the sample", all(idx %in% seq_len(11L)))
check(
  "moving-block draw preserves order inside each block",
  all(diff(idx[1:4]) == 1L) && all(diff(idx[5:8]) == 1L) &&
    all(diff(idx[9:11]) == 1L)
)
set.seed(91)
check("moving-block draw is controlled by caller RNG", identical(idx, mbb_index(11L, 4L)))
check("oversized block returns the full sample", identical(mbb_index(5L, 9L), 1:5))

mbb_run_a <- paper_run_mbb_draws(
  n_draws = 4L,
  sample_size = 11L,
  block_length = 4L,
  truncation = 8L,
  seed = 333L,
  draw = function(index, draw_id) {
    c(draw_id = draw_id, total = sum(index))
  }
)
mbb_run_b <- paper_run_mbb_draws(
  n_draws = 4L,
  sample_size = 11L,
  block_length = 4L,
  truncation = 8L,
  seed = 333L,
  draw = function(index, draw_id) {
    c(draw_id = draw_id, total = sum(index))
  }
)
check(
  "moving-block runner pre-draws a reproducible index stream",
  identical(mbb_run_a$indices, mbb_run_b$indices) &&
    identical(mbb_run_a$draws, mbb_run_b$draws) &&
    all(lengths(mbb_run_a$indices) == 8L)
)
mbb_progress <- integer()
mbb_failed <- paper_run_mbb_draws(
  n_draws = 3L,
  sample_size = 9L,
  block_length = 3L,
  seed = 444L,
  draw = function(index, draw_id) {
    if (draw_id == 2L) stop("fixture draw failed")
    sum(index)
  },
  progress = function(draw_id, n_draws, started_at) {
    mbb_progress <<- c(mbb_progress, draw_id)
  }
)
check(
  "moving-block runner captures failures and sequential progress",
  mbb_failed$n_failed == 1L &&
    identical(mbb_failed$draws[[2L]], "fixture draw failed") &&
    identical(mbb_progress, 1:3)
)
rm(mbb_run_a, mbb_run_b, mbb_progress, mbb_failed)

summary_row <- compute_summary_stats(c(1, 2, NA, 4), "x", compute_ac = FALSE)
check("summary statistics count finite observations", summary_row$N == 3L)
check("summary statistics preserve the variable label", summary_row$Variable == "x")
check("autocorrelations are optional", !"AC1" %in% names(summary_row))

# Seeded series make the stationarity-null directions decisive.
n <- 130
set.seed(42)
x_stationary <- as.numeric(arima.sim(list(ar = 0.5), n))
set.seed(7)
x_random_walk <- cumsum(rnorm(n))
res_st <- perform_stationarity_tests(x_stationary, "stationary")
res_rw <- perform_stationarity_tests(x_random_walk, "random_walk")
check("stationary series has a small ADF p-value", res_st$ADF_pval < 1e-6)
check(
  "stationary series has a clamped KPSS p-value",
  isTRUE(all.equal(res_st$KPSS_pval, 0.10))
)
check("random walk has a large ADF p-value", res_rw$ADF_pval > 0.10)
check(
  "random walk has a clamped KPSS p-value",
  isTRUE(all.equal(res_rw$KPSS_pval, 0.01))
)
adf_kpss_p <- c(
  res_st$ADF_pval, res_st$KPSS_pval, res_rw$ADF_pval, res_rw$KPSS_pval
)
check("stationarity p-values lie in the unit interval", all(adf_kpss_p > 0 & adf_kpss_p < 1))

# MacKinnon response surfaces recover the nominal urca critical-value levels.
adf <- urca::ur.df(x_stationary, type = "drift", selectlags = "AIC")
p_adf_cv <- vapply(as.numeric(adf@cval[1, ]), function(cv) {
  urca::punitroot(cv, N = n, trend = "c", statistic = "t")
}, numeric(1))
check(
  "ADF critical values recover their nominal levels",
  all(abs(p_adf_cv - c(0.01, 0.05, 0.10)) < 0.005)
)
pp <- urca::ur.pp(x_stationary, type = "Z-tau", model = "constant")
p_pp_cv <- vapply(as.numeric(pp@cval), function(cv) {
  urca::punitroot(cv, N = n, trend = "c", statistic = "t")
}, numeric(1))
check(
  "PP critical values recover their nominal levels",
  all(abs(p_pp_cv - c(0.01, 0.05, 0.10)) < 0.005)
)
p_grid <- vapply(c(-4, -3, -2, -1), function(q) {
  urca::punitroot(q, N = n, trend = "c", statistic = "t")
}, numeric(1))
check("unit-root p-values are monotone in the statistic", all(diff(p_grid) > 0))

# KPSS interpolation is exact at knots, monotone between them, and clamped outside.
kpss_cval <- urca::ur.kpss(x_stationary, type = "mu")@cval
check(
  "KPSS critical values match the reference table",
  isTRUE(all.equal(as.numeric(kpss_cval), c(0.347, 0.463, 0.574, 0.739)))
)
p_knots <- vapply(as.numeric(kpss_cval), function(stat) {
  kpss_pvalue(stat, kpss_cval)
}, numeric(1))
check(
  "KPSS interpolation is exact at the table knots",
  isTRUE(all.equal(p_knots, c(0.10, 0.05, 0.025, 0.01)))
)
stat_grid <- seq(0.05, 1.5, by = 0.01)
p_interp <- vapply(stat_grid, function(stat) {
  kpss_pvalue(stat, kpss_cval)
}, numeric(1))
check("KPSS interpolation is monotone", all(diff(p_interp) <= 1e-12))
check("KPSS interpolation is clamped", all(p_interp >= 0.01 & p_interp <= 0.10))

check("complete data pass validation", check_data_completeness(data.frame(x = 1:3)))
missing_error <- tryCatch(
  check_data_completeness(data.frame(x = c(1, NA))),
  error = function(error) conditionMessage(error)
)
check(
  "missing data fail validation and name the column",
  is.character(missing_error) && grepl("x", missing_error, fixed = TRUE)
)

paper_source_once(paper_path("tests", "support", "analysis_contract_checks.R"))
paper_source_once(paper_path("tests", "support", "inference_control_checks.R"))
paper_source_once(paper_path("tests", "support", "artifact_lifecycle_checks.R"))
paper_source_once(paper_path("tests", "support", "shared_artifact_checks.R"))

.test$finish()
