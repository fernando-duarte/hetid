# Per-maturity SDF-news and expected-SDF approximation-error variance bounds
# across the quarterly maturity grid. Run via run_pipeline.R, which defines the
# shared grids. The news bound is the pointwise min of the envelope bound
# U_i = (1/4) * c_hat_i * (k_hat_i + k2_hat_i) and the two-leg
# first-order-cancelled bound of compute_news_q_bound; the expected-SDF bound
# is the min{(1/4) C K, Var(q)} of compute_expected_sdf_variance_bound. Every
# bound is computed from the ACM yields and term premia alone.

yields <- quarterly_acm_inputs$yields
term_premia <- quarterly_acm_inputs$term_premia

# Valid bound indices are positive multiples of the quarterly step up to the
# step-trimmed maximum: seq(3, 117, 3) = 39 maturities under the quarterly clock.
vb_mats <- seq(step_qtr, hetid::effective_max_maturity(step_qtr), by = step_qtr)
vb_series <- function(fn) {
  vapply(
    vb_mats,
    \(i) fn(yields, term_premia, i = i, step = step_qtr),
    numeric(1)
  )
}
news_env <- vb_series(hetid::compute_variance_bound)
news_q <- vb_series(hetid::compute_news_q_bound)
esdf_bounds <- vb_series(hetid::compute_expected_sdf_variance_bound)
news_min <- pmin(news_env, news_q)

# Fail loud per series so a failure names the offending bound. Reported series
# must be finite and strictly positive (log-scale figure, sci-notation cells);
# news_q may be Inf (its documented conservative fallback, which just loses
# the pmin) but never NA/NaN or nonpositive.
stopifnot(
  "news envelope bound must be finite and positive at every maturity" =
    all(is.finite(news_env) & news_env > 0),
  "news q-bound must be positive and non-NA at every maturity" =
    all(!is.na(news_q) & news_q > 0),
  "reported news bound must be finite and positive at every maturity" =
    all(is.finite(news_min) & news_min > 0),
  "expected-SDF bound must be finite and positive at every maturity" =
    all(is.finite(esdf_bounds) & esdf_bounds > 0)
)

cat(
  "variance bounds:", length(vb_mats), "maturities;",
  "news q-bound binds at", sum(news_q < news_env), "\n"
)

variance_bounds_df <- data.frame(
  Maturity = vb_mats,
  Variance_Bound = news_min,
  Expected_SDF_Bound = esdf_bounds
)
vb_summary_stats <- function(x) {
  c(
    Mean = mean(x),
    Median = stats::median(x),
    Minimum = min(x),
    Maximum = max(x),
    "Standard Deviation" = stats::sd(x)
  )
}
variance_bounds_summary <- cbind(
  "SDF news" = vb_summary_stats(news_min),
  "Expected SDF" = vb_summary_stats(esdf_bounds)
)

# the ACM inputs were only function arguments here
rm(
  yields, term_premia, vb_series, news_env, news_q, esdf_bounds, news_min,
  vb_summary_stats, vb_mats
)
