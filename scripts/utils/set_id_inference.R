# Sampling-uncertainty helpers for the set-identified coefficients: percentile
# bands over bootstrap draws and Imbens-Manski (2004) confidence intervals for
# the true coefficient, anchored at the exact identified-set endpoints with
# bootstrap standard errors. Consumed by scripts-paper/set_id_bootstrap.R;
# tested in scripts/utils/tests/test_set_id_inference.R.

# Median and 90% percentile band of the finite draws (the house p05/p95 form).
boot_band <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(c(median = NA_real_, p05 = NA_real_, p95 = NA_real_, n = 0))
  }
  c(
    median = stats::median(x),
    p05 = unname(stats::quantile(x, 0.05)),
    p95 = unname(stats::quantile(x, 0.95)),
    n = length(x)
  )
}

# Imbens-Manski critical value: c solves Phi(c + width/s) - Phi(-c) = 1 - alpha
# with s the larger endpoint standard error. Interpolates between the two-sided
# normal quantile at width 0 and the one-sided quantile as the set widens.
# A degenerate s falls back to the one-sided value (the interval then adds
# nothing beyond the exact set).
im_critical <- function(width, se_lower, se_upper, alpha = 0.10) {
  stopifnot(length(width) == 1L, alpha > 0, alpha < 1)
  s <- max(se_lower, se_upper)
  if (!is.finite(s) || s <= 0) {
    return(stats::qnorm(1 - alpha))
  }
  delta <- max(width, 0) / s
  f <- function(c) stats::pnorm(c + delta) - stats::pnorm(-c) - (1 - alpha)
  stats::uniroot(f, interval = c(0, 10), tol = 1e-10)$root
}

# Endpoint standard errors and Imbens-Manski interval for every coefficient at
# one display tau. lower/upper are B x n_coef matrices of endpoint draws (NA
# marks a failed or uncertified draw); set_table is the full-sample
# coef/set_lower/set_upper/status frame the draws re-estimate. Rows without a
# certified nondegenerate full-sample interval, or with fewer than min_reps
# finite draw pairs, keep their standard errors but report NA intervals
# (degenerate width-0 rows are point-identified; their cells stay blank).
endpoint_inference <- function(lower, upper, set_table, alpha = 0.10,
                               min_reps = nrow(lower) %/% 2L) {
  stopifnot(identical(dim(lower), dim(upper)), ncol(lower) == nrow(set_table))
  rows <- lapply(seq_len(nrow(set_table)), function(k) {
    ok <- is.finite(lower[, k]) & is.finite(upper[, k])
    se_lo <- if (any(ok)) stats::sd(lower[ok, k]) else NA_real_
    se_up <- if (any(ok)) stats::sd(upper[ok, k]) else NA_real_
    width <- set_table$set_upper[k] - set_table$set_lower[k]
    usable <- set_table$status[k] == "bounded" && is.finite(width) &&
      width > 0 && sum(ok) >= min_reps &&
      is.finite(se_lo) && is.finite(se_up)
    if (usable) {
      c_im <- im_critical(width, se_lo, se_up, alpha)
      im_lo <- set_table$set_lower[k] - c_im * se_lo
      im_up <- set_table$set_upper[k] + c_im * se_up
    } else {
      im_lo <- NA_real_
      im_up <- NA_real_
    }
    data.frame(
      coef = set_table$coef[k], se_lower = se_lo, se_upper = se_up,
      n_finite = sum(ok), im_lower = im_lo, im_upper = im_up,
      row.names = NULL, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
