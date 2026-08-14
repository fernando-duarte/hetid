# Offline checks for the combined slack panel's two families. Run from the
# package root:
#   Rscript scripts-paper/tests/figures/fitted_volatility/test_tau_sweep.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "tau_sweep_plot.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "envelope.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "tau_sweep_extra_panels.R"
))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# Two nested fixture envelopes: a common exponent path with a half-width per
# slack, so the log-variance band width is exactly twice that half-width and the
# variance band width is the exponential of the same endpoints.
sweep_n <- 24L
sweep_qtr <- seq(as.Date("1990-03-31"), by = "quarter", length.out = sweep_n)
sweep_eta <- sin(seq_len(sweep_n) / 4) - 0.3
sweep_env <- function(tau, half) {
  rows <- data.frame(
    qtr = sweep_qtr, date = sweep_qtr,
    log_variance_lower = sweep_eta - half,
    log_variance_upper = sweep_eta + half,
    log_variance_point = sweep_eta,
    lower_status = PAPER_ENDPOINT_STATUS[["bounded"]],
    upper_status = PAPER_ENDPOINT_STATUS[["bounded"]],
    row.names = NULL
  )
  for (side in c("lower", "upper", "point")) {
    eta <- rows[[paste0("log_variance_", side)]]
    rows[[paste0("variance_", side)]] <- exp(eta)
    rows[[paste0("volatility_", side)]] <- exp(0.5 * eta)
  }
  list(metadata = list(tau = tau), data = rows)
}
sweep_halves <- c(0.15, 0.45)
sweep_envs <- list(
  sweep_env(0.05, sweep_halves[1]), sweep_env(0.20, sweep_halves[2])
)

sweep_render <- function(log_variance, extra_line = NULL) {
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  widths <- logvar_tau_sweep_render(
    sweep_envs, path,
    log_variance = log_variance, extra_line = extra_line
  )
  list(
    widths = widths,
    svg = paste(readLines(path, warn = FALSE), collapse = ""),
    size = file.size(path)
  )
}

# The exponent family plots the endpoints untransformed, so its band widths are
# the fixture half-widths doubled, with no halving anywhere in the path.
sweep_log <- sweep_render(TRUE)
check(
  "the log-variance panel plots the exponent itself",
  max(abs(sweep_log$widths - 2 * sweep_halves)) < 1e-12
)

# The exponential family plots exp(eta), so its widths are the medians of the
# exponentiated endpoints rather than an affine image of the fixture.
sweep_exp <- sweep_render(FALSE)
sweep_expected <- vapply(sweep_halves, function(half) {
  stats::median(exp(sweep_eta + half) - exp(sweep_eta - half))
}, numeric(1))
check(
  "the exponential panel plots exp of the exponent",
  max(abs(sweep_exp$widths - sweep_expected)) < 1e-12
)

# svglite emits real <text>, so each panel's axis title is readable in the file
# and has to be the one naming what that panel actually draws.
check(
  "each family carries its own axis title and not its sibling's",
  grepl(LOGVAR_FITTED_VOL_Y_LABEL_TEX, sweep_log$svg, fixed = TRUE) &&
    !grepl(LOGVAR_FITTED_VOL_Y_LABEL_EXP_TEX, sweep_log$svg, fixed = TRUE) &&
    grepl(LOGVAR_FITTED_VOL_Y_LABEL_EXP_TEX, sweep_exp$svg, fixed = TRUE)
)

# Neither panel transforms the axis, so the exponent panel keeps the dates whose
# value is negative rather than dropping them to a log scale's undefined side.
check(
  "the exponent panel survives a series that crosses zero",
  any(sweep_eta < 0) && sweep_log$size > 0L && sweep_exp$size > 0L
)

# The OLS benchmark line, checked against stats::lm rather than against the
# projection matrix the implementation uses, so the check is independent of that
# code path: the value has to be the fitted log variance with the intercept term
# left out, at the mean equation's OLS news coefficients.
ols_pc_cols <- PAPER_ANALYSIS_CONTRACT$model$return_pc_cols
set.seed(7)
ols_n <- 50L
ols_pcr <- matrix(
  stats::rnorm(ols_n * length(ols_pc_cols)),
  nrow = ols_n,
  dimnames = list(NULL, ols_pc_cols)
)
ols_w2 <- matrix(stats::rnorm(ols_n * 2L), nrow = ols_n)
ols_b <- c(0.4, -0.25)
ols_w1 <- drop(ols_w2 %*% ols_b) + stats::rnorm(ols_n)
ols_inputs <- list(
  qtr = seq(as.Date("1990-03-31"), by = "quarter", length.out = ols_n),
  w1 = ols_w1, w2 = ols_w2, pcr = ols_pcr
)
ols_mean_eq <- list(theta_table = list(ols = ols_b))
ols_x <- logvar_fitted_vol_design(ols_pcr)
ols_line <- logvar_tau_sweep_ols_line(ols_mean_eq, ols_inputs, ols_x)
ols_eps <- drop(ols_w1 - ols_w2 %*% ols_b)
ols_lm <- stats::lm(log(ols_eps^2) ~ ols_pcr)
ols_expected <- drop(ols_pcr %*% stats::coef(ols_lm)[-1L])
check(
  "the OLS benchmark line is the intercept-free log-OLS fit at b_ols",
  nrow(ols_line) == ols_n &&
    identical(ols_line$date, as.Date(ols_inputs$qtr)) &&
    max(abs(ols_line$value - ols_expected)) < 1e-9
)

# Its own key and colour, and it must not disturb the bands it is drawn over.
ols_panel <- sweep_render(TRUE, extra_line = ols_line)
check(
  "the extra line adds a key without changing the bands",
  identical(ols_panel$widths, sweep_log$widths) &&
    grepl(LOGVAR_TAU_SWEEP_OLS_KEY, ols_panel$svg, fixed = TRUE) &&
    !grepl(LOGVAR_TAU_SWEEP_OLS_KEY, sweep_log$svg, fixed = TRUE)
)

.test$finish()
