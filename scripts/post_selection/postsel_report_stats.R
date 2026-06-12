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

# Registered K grid for the scoped validation claim (K4 rescope
# round, docs/postsel-sim-k4-preregistration.md). The affirmative
# wording is gated on a FULL verdict whose recorded grid AND
# realized cells both equal this registered grid -- fail-closed for
# legacy artifacts (no k_grid), subsets, supersets, and grid/cell
# mismatches. The PRINTED scope always comes from the verdict
# artifact's own grid, never from prose.
POSTSEL_K_SCOPE <- c(2L, 4L)

# Grid-exact scope phrase derived ONLY from the verdict artifact's
# recorded grid: names the tested grid, the application-parity
# bound, and the explicit non-claims for untested interior K and for
# K above the grid.
postsel_k_scope_phrase <- function(k_grid) {
  ks <- sort(unique(as.integer(k_grid)))
  k_max <- max(ks)
  untested <- setdiff(seq.int(min(ks), k_max), ks)
  sprintf(
    paste0(
      "on the registered K grid {%s} (application-parity scope up",
      " to K = %d; no claim for untested K%s, or for K > %d)"
    ),
    paste(ks, collapse = ", "), k_max,
    if (length(untested) > 0L) {
      sprintf(", e.g. K = %s", paste(untested, collapse = ", "))
    } else {
      ""
    },
    k_max
  )
}

# TRUE iff the simulation artifact's RECORDED grid (settings$k_grid)
# AND its REALIZED cells (results$k_inst) both equal the registered
# scope as sorted sets. The report accepts arbitrary
# HETID_POSTSEL_SIM_SOURCE artifacts, so the self-declared grid
# alone is never trusted.
sim_k_scope_ok <- function(sim) {
  k_grid <- sim$settings$k_grid
  k_cells <- sim$results$k_inst
  !is.null(k_grid) && !is.null(k_cells) &&
    identical(sort(unique(as.integer(k_grid))), POSTSEL_K_SCOPE) &&
    identical(sort(unique(as.integer(k_cells))), POSTSEL_K_SCOPE)
}

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
