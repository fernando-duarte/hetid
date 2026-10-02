# One log-projection preparation for every log-scale estimator: the full
# mean sample from the identified-set fit, the volatility rows by quarter,
# and the raw return PCs (centered by the package), under the mean system's
# news restriction (imposed-null news need not have mean zero). Checked against the
# log-OLS inputs so the two centerings and samples cannot drift.
# Definitions only; sourced by log_ols/run.R.

logvar_log_projection_prep <- function(mean_eq, rows, raw_pcs, pcr, w1_lv) {
  prep <- hetid::prepare_log_projection(
    w1 = mean_eq$w1, w2 = mean_eq$w2, x_var = raw_pcs,
    mean_ids = mean_eq$qtr, volatility_ids = rows$qtr,
    impose_null = mean_eq$impose_null
  )
  stopifnot(
    isTRUE(all.equal(unclass(prep$x_centered), unclass(pcr),
      tolerance = 1e-12, check.attributes = FALSE
    )),
    identical(unname(prep$w1), unname(w1_lv))
  )
  prep
}
