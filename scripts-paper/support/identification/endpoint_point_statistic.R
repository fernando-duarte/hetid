# The tau = 0 cell for BOTH panels: the full-sample point estimate over the
# robust bootstrap scale of that same point's draws, with a two-sided normal
# p-value. Exactly consistent with the Wald interval point +/- z_{1-alpha/2}*se,
# which excludes zero if and only if |t| > z_{1-alpha/2}.
#
# The denominator is the MAD rather than the sample standard deviation because
# the tau = 0 estimator solves a linear system in estimated moments: resamples in
# which that system is nearly singular produce arbitrarily large values, so the
# standard deviation does not settle as B grows while the MAD does. In this
# application the ratio runs 10 to 20 for the set-identified block against about
# 1.02 for the point-identified one.
#
# Panel A feeds the closed-form tau = 0 solution's draws; Panel B feeds the
# per-draw direct quasi-maximum-likelihood evaluations at each draw's tau = 0
# news vector. Both pass a status matrix in the shared four-word vocabulary, with
# "unbounded" forbidden: a point evaluation cannot diverge, so its presence is an
# implementation error rather than a data condition. Stars are left to the
# renderers, which already have sig_stars in scope, so this stays free of a
# reporting dependency.

paper_source_once(paper_path(
  "support", "identification", "endpoint_targets.R"
))

point_t_statistic <- function(point_hat, point_draws, point_status,
                              min_reps = boot_min_reps(nrow(point_draws)),
                              stability =
                                PAPER_ANALYSIS_CONTRACT$inference$stability_share) {
  stopifnot(
    length(point_hat) == ncol(point_draws),
    identical(dim(point_status), dim(point_draws))
  )
  paper_endpoint_status_validate(as.vector(point_status))
  stopifnot(!any(point_status == PAPER_ENDPOINT_STATUS[["unbounded"]]))
  rows <- lapply(seq_along(point_hat), function(k) {
    status <- point_status[, k]
    ok <- is.finite(point_draws[, k]) &
      status == PAPER_ENDPOINT_STATUS[["bounded"]]
    n_valid <- sum(status != PAPER_ENDPOINT_STATUS[["failed"]])
    frac <- if (n_valid > 0L) sum(ok) / n_valid else 0
    se <- if (sum(ok) >= 2L) robust_scale(point_draws[ok, k]) else NA_real_
    # the gate is the same two-part policy the interval cells use: an absolute
    # count against B, then a bounded share over the non-failed draws
    reason <- if (!is.finite(point_hat[[k]])) {
      "full-sample point not available"
    } else if (sum(ok) < min_reps) {
      "insufficient bounded draws"
    } else if (frac < stability) {
      "boundedness unstable across draws"
    } else if (!is.finite(se) || se <= 0) {
      "degenerate point scale"
    } else {
      "reported"
    }
    statistic <- if (identical(reason, "reported")) {
      point_hat[[k]] / se
    } else {
      NA_real_
    }
    data.frame(
      coef = colnames(point_draws)[[k]], point = point_hat[[k]], se = se,
      statistic = statistic, p_value = 2 * stats::pnorm(-abs(statistic)),
      n_bounded = sum(status == PAPER_ENDPOINT_STATUS[["bounded"]]),
      n_unbounded = sum(status == PAPER_ENDPOINT_STATUS[["unbounded"]]),
      n_unreliable = sum(status == PAPER_ENDPOINT_STATUS[["unreliable"]]),
      n_failed = sum(status == PAPER_ENDPOINT_STATUS[["failed"]]),
      n_valid_point = sum(ok), n_non_failed = n_valid, frac_bounded = frac,
      min_reps = min_reps, reason = reason,
      row.names = NULL, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
