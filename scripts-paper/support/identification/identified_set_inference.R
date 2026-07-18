# Sampling-uncertainty helpers for the set-identified coefficients: percentile
# bands over bootstrap draws, and confidence intervals for the true
# coefficient anchored at the exact identified-set endpoints with robust
# (MAD-based) endpoint scales. The interval uses a Stoye (2009)-style joint
# calibration when the endpoint correlation is estimable, falling back to the
# Imbens-Manski (2004) interpolation otherwise. Consumed by
# scripts-paper/mean_equation/inference/run_bootstrap.R and tested by the paper
# inference suite.
paper_source_once(paper_path(
  "support", "identification", "inference_calibration.R"
))

# Median and nominal percentile band of the finite draws.
boot_band <- function(x, alpha = PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha) {
  stopifnot(alpha > 0, alpha < 1)
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(c(
      median = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      n = 0
    ))
  }
  c(
    median = stats::median(x),
    lower = unname(stats::quantile(x, alpha / 2)),
    upper = unname(stats::quantile(x, 1 - alpha / 2)),
    n = length(x)
  )
}
# Contract-owned reliability threshold shared by every rendering gate (the
# set cells, the tau = 0 point interval, and the diagnostics table).
boot_min_reps <- function(
  b,
  inference = PAPER_ANALYSIS_CONTRACT$inference
) {
  ceiling(b * inference$minimum_valid_draw_share)
}
# Robust endpoint scales and a Stoye-calibrated nominal interval for every
# coefficient at one display tau. lower/upper are B x n_coef matrices of
# endpoint draws (NA marks a failed or uncertified draw); set_table is the
# full-sample coef/set_lower/set_upper/status frame the draws re-estimate.
# Rows without a certified nondegenerate full-sample interval, or with fewer
# than min_reps finite draw pairs, keep their scales but report NA intervals
# (degenerate width-0 rows are point-identified; their cells stay blank).
# When the endpoint correlation cannot be estimated the Imbens-Manski
# interpolation is used in place of the Stoye calibration.
endpoint_inference <- function(lower, upper, set_table,
                               alpha =
                                 PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha,
                               min_reps = boot_min_reps(nrow(lower)),
                               control = PAPER_INFERENCE_SEARCH_CONTROL) {
  stopifnot(identical(dim(lower), dim(upper)), ncol(lower) == nrow(set_table))
  rows <- lapply(seq_len(nrow(set_table)), function(k) {
    ok <- is.finite(lower[, k]) & is.finite(upper[, k])
    se_lo <- robust_scale(lower[ok, k])
    se_up <- robust_scale(upper[ok, k])
    # width uncertainty from the paired draws, so the endpoint correlation
    # is handled exactly (correlated endpoints shift more than they stretch)
    width_draws <- upper[ok, k] - lower[ok, k]
    se_w <- robust_scale(width_draws)
    width_band <- boot_band(width_draws, alpha)
    rho <- robust_endpoint_cor(lower[, k], upper[, k], control)
    width <- set_table$set_upper[k] - set_table$set_lower[k]
    usable <- set_table$status[k] == PAPER_ENDPOINT_STATUS[["bounded"]] && is.finite(width) &&
      width > 0 && sum(ok) >= min_reps &&
      is.finite(se_lo) && is.finite(se_up) && se_lo > 0 && se_up > 0
    if (usable) {
      c_im <- im_critical(width, se_lo, se_up, alpha, control)
      c_st <- if (is.finite(rho)) {
        stoye_critical(width, se_lo, se_up, rho, alpha, control)
      } else {
        c_im
      }
      ci_lo <- set_table$set_lower[k] - c_st * se_lo
      ci_up <- set_table$set_upper[k] + c_st * se_up
    } else {
      c_im <- NA_real_
      c_st <- NA_real_
      ci_lo <- NA_real_
      ci_up <- NA_real_
    }
    data.frame(
      coef = set_table$coef[k], se_lower = se_lo, se_upper = se_up,
      se_width = se_w, width_lower = width_band[["lower"]],
      width_upper = width_band[["upper"]],
      rho = rho, n_finite = sum(ok), c_im = c_im, c_stoye = c_st,
      ci_lower = ci_lo, ci_upper = ci_up,
      row.names = NULL, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Robust nominal interval for the tau = 0 point estimates: the closed-form
# point plus/minus the two-sided normal quantile times the robust scale of
# the point draws. The interval is suppressed when the point or its scale is
# degenerate or fewer than min_reps draws deliver a finite point (an interval
# built from a full-rank minority would hide the deficiency it rides on).
point_inference <- function(point_hat, point_draws,
                            alpha =
                              PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha,
                            min_reps = boot_min_reps(nrow(point_draws))) {
  stopifnot(length(point_hat) == ncol(point_draws), alpha > 0, alpha < 1)
  se <- apply(point_draws, 2, robust_scale)
  n_finite <- colSums(is.finite(point_draws))
  ok <- is.finite(point_hat) & is.finite(se) & se > 0 & n_finite >= min_reps
  z <- stats::qnorm(1 - alpha / 2)
  data.frame(
    coef = colnames(point_draws), se = se, n_finite = n_finite,
    lower = ifelse(ok, point_hat - z * se, NA_real_),
    upper = ifelse(ok, point_hat + z * se, NA_real_),
    row.names = NULL, stringsAsFactors = FALSE
  )
}
