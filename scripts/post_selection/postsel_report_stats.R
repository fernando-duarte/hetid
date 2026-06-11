# Post-selection report statistics: simulation coverage aggregation,
# the pre-specified acceptance verdict, and the exit-status rule.
# The margins are FROZEN by the plan (docs/superpowers/plans/
# 2026-06-12-post-selection-split-inference.md, decision D9); DGP
# tuning during the pilot may not touch them. The verdict is computed
# from the artifacts alone, the text layer prints it verbatim, and
# the report driver fails the process on a failing verdict (honesty
# precedence: a FAIL is reported, never softened, never silent).

POSTSEL_ACCEPT <- list(
  # opt_full must trail fixed_full by at least this at the largest K
  degrade_margin = 0.10,
  # |split - fixed_e| must stay within this at every K
  match_margin = 0.07,
  # split must beat self_e by at least this at the largest K
  repair_margin = 0.10
)

# Mean coverage with applicable-cell denominators. Membership is
# well-defined for unbounded sets too, so every rep contributes; n is
# carried per (K, arm) cell rather than assumed.
aggregate_sim_coverage <- function(results) {
  agg <- aggregate(covered ~ k_inst + arm, data = results, FUN = mean)
  n <- aggregate(covered ~ k_inst + arm, data = results, FUN = length)
  names(n)[names(n) == "covered"] <- "n"
  agg <- merge(agg, n, by = c("k_inst", "arm"))
  agg$se <- sqrt(agg$covered * (1 - agg$covered) / agg$n)
  agg[order(agg$k_inst, agg$arm), ]
}

# Acceptance verdict on the FULL simulation grid. Returns the named
# checks plus the wide coverage table and margins so the text layer
# prints evidence, not just verdicts.
sim_acceptance <- function(coverage) {
  wide <- tidyr::pivot_wider(
    coverage[, c("k_inst", "arm", "covered")],
    id_cols = "k_inst", names_from = "arm", values_from = "covered"
  )
  wide <- as.data.frame(wide[order(wide$k_inst), ])
  k_hi <- wide[nrow(wide), ]
  k_lo <- wide[1, ]
  list(
    checks = list(
      selection_degrades_coverage =
        k_hi$opt_full <=
          k_hi$fixed_full - POSTSEL_ACCEPT$degrade_margin,
      degradation_grows_with_instruments =
        k_hi$opt_full < k_lo$opt_full,
      split_matches_fixed_benchmark = all(
        abs(wide$split - wide$fixed_e) <= POSTSEL_ACCEPT$match_margin
      ),
      split_repairs_inblock_selection =
        k_hi$split >= k_hi$self_e + POSTSEL_ACCEPT$repair_margin
    ),
    wide = wide,
    margins = POSTSEL_ACCEPT
  )
}

# Exit status for the report driver: nonzero exactly when a FULL
# simulation verdict exists and any pre-specified check failed
# (report-and-stop made mechanical; quick/no-sim runs exit 0 -- they
# are honesty-suffixed artifacts, not failures).
postsel_exit_status <- function(acceptance) {
  if (!is.null(acceptance) && !all(unlist(acceptance$checks))) {
    1L
  } else {
    0L
  }
}
