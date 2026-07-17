# Per-maturity SDF-news approximation-error variance bounds across the quarterly
# maturity grid. Run via run_pipeline.R, which defines the shared grids. Each bound
# U_i = (1/4) * c_hat_i * (k_hat_i + k2_hat_i) is computed by compute_variance_bound
# from the ACM yields and term premia alone; every intermediate is internal.

acm <- hetid::extract_acm_data(
  data_types = c("yields", "term_premia"),
  maturities = all_mats,
  frequency = "quarterly",
  auto_download = FALSE,
  source = "auto"
)
yields <- acm[, paste0("y", all_mats)]
term_premia <- acm[, paste0("tp", all_mats)]

# Valid bound indices are positive multiples of the quarterly step up to the
# step-trimmed maximum: seq(3, 117, 3) = 39 maturities under the quarterly clock.
vb_mats <- seq(step_qtr, hetid::effective_max_maturity(step_qtr), by = step_qtr)
variance_bounds <- vapply(
  vb_mats,
  \(i) hetid::compute_variance_bound(yields, term_premia, i = i, step = step_qtr),
  numeric(1)
)
# Fail loud on a missing bound: na.rm below would summarise over a silently
# smaller grid than the "across maturities" caption claims. Matches the
# load_term_premia contract of raising rather than returning a sentinel.
stopifnot(
  "variance bound is NA for at least one maturity" = !anyNA(variance_bounds)
)

variance_bounds_df <- data.frame(
  Maturity = vb_mats,
  Variance_Bound = variance_bounds
)
variance_bounds_summary <- c(
  Mean = mean(variance_bounds),
  Median = stats::median(variance_bounds),
  Minimum = min(variance_bounds),
  Maximum = max(variance_bounds),
  "Standard Deviation" = stats::sd(variance_bounds)
)

# the ACM inputs were only function arguments here
rm(acm, yields, term_premia, variance_bounds, vb_mats)
