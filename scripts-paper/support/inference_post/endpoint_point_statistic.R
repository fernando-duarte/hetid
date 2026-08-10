# The tau = 0 cell for BOTH panels: the full-sample point estimate over the
# robust bootstrap scale of that same point's draws, with two calibrations
# computed beside it. PAPER_POINT_STAR_BASIS picks the one the tables star on;
# point_star_p reads it.
#
# WHAT EACH BUYS. p_value_normal is the two-sided normal tail of the reported
# ratio and is the published default, so the stars follow from the number
# printed beneath the estimate and the column reads like the OLS column beside
# it. p_value is the draws' own absolute-deviation tail, which shares its
# reference distribution with the tau > 0 cells: those take their critical value
# from the empirical root distribution, so starring on the normal leaves two
# reference distributions in one table. The normal is also the more generous
# side, because the root distribution is heavy through its body, with a
# ninetieth percentile near 3.4 against the normal's 1.645.
#
# THIS IS AN EXACT SPECIALIZATION AT ZERO WIDTH, NOT A LIMIT. At a
# point-identified cell L = U, so the inward roots satisfy z_U = -z_L and Target
# S/P collapse to |z_L|. That is an algebraic identity at tau = 0, not a
# statement that anything converges as tau falls: active sets and status maps can
# be discontinuous there. Do not describe it as a limit.
#
# THE SCALE CANCELS. Comparing (point*_b - point_hat)/se against point_hat/se is
# the same as comparing |point*_b - point_hat| against |point_hat|, so the
# p-value does not depend on se at all. It is therefore NOT studentised, whatever
# the presence of a denominator suggests; se still sets the reported statistic
# and the interval, but not the calibration.
#
# The absolute-deviation test is tail-unbalanced under skew, so the two
# directional tails ride along beside it. Because that tail is a rank and the
# reported ratio is not, the bootstrap basis can order the two differently: a
# smaller ratio stars where a larger one does not when its draws are thinner
# relative to their own MAD. That ordering is what the normal basis avoids.
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
  "support", "inference_post", "endpoint_targets.R"
))

# Two-sided absolute-deviation p-value against the bootstrap's own distribution,
# plus its two directional halves. The finite-B rule adds one to numerator and
# denominator so a p-value can never be zero: with B draws the smallest
# attainable value is 1/(B+1), and reporting 0 would assert more than B draws can
# support. `draws` is already masked to the usable draws.
point_bootstrap_p <- function(draws, point_hat, reason) {
  blank <- list(p_value = NA_real_, p_lower = NA_real_, p_upper = NA_real_)
  if (!identical(reason, "reported") || !length(draws)) {
    return(blank)
  }
  dev <- draws - point_hat
  obs <- abs(point_hat)
  n <- length(dev)
  list(
    p_value = (1 + sum(abs(dev) >= obs)) / (n + 1),
    p_lower = (1 + sum(dev <= -obs)) / (n + 1),
    p_upper = (1 + sum(dev >= obs)) / (n + 1)
  )
}

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
    boot <- point_bootstrap_p(point_draws[ok, k], point_hat[[k]], reason)
    data.frame(
      coef = colnames(point_draws)[[k]], point = point_hat[[k]], se = se,
      statistic = statistic, p_value = boot$p_value,
      p_value_normal = 2 * stats::pnorm(-abs(statistic)),
      p_lower = boot$p_lower, p_upper = boot$p_upper,
      # the vocabulary is enumerated once, where it is defined
      as.list(stats::setNames(
        paper_endpoint_status_counts(status),
        paste0("n_", names(PAPER_ENDPOINT_STATUS))
      )),
      n_valid_point = sum(ok), n_non_failed = n_valid, frac_bounded = frac,
      min_reps = min_reps, reason = reason,
      row.names = NULL, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
