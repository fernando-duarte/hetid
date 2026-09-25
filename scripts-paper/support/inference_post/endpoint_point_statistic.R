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

point_t_statistic <- function(point_hat, point_draws, point_status,
                              min_reps = boot_min_reps(nrow(point_draws)),
                              stability = PAPER_ANALYSIS_CONTRACT$inference$stability_share) {
  if (is.null(names(point_hat))) names(point_hat) <- colnames(point_draws)
  if (is.null(colnames(point_status))) colnames(point_status) <- colnames(point_draws)
  hetid::bootstrap_point_statistics(point_hat, point_draws, point_status, min_reps, stability)
}
