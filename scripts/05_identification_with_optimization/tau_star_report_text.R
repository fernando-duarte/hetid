# Combined plain-text summary for the tau* analysis, written for an economist
# reader who knows the math/econ but not the code. Bottom line first, then
# refreshers, per-mode results, the honest reading of the curvature
# diagnostic, artifact pointers, and numerical notes. All numbers are
# computed from the saved results, never hard-coded. Formatting helpers live
# in tau_star_report_utils.R.

build_tau_star_summary <- function(results) {
  mat <- results$maturities
  fac <- results$factors
  vf_m <- .ts_of(mat, "VFCI (rank-1)")
  op_m <- .ts_of(mat, "optimized")
  vf_f <- .ts_of(fac, "VFCI (rank-1)")
  rf_f <- .ts_of(fac, "reduced-form (rank-3)")
  op_f <- .ts_of(fac, "optimized")
  ratio_m <- op_m$tau_star / vf_m$tau_star

  vf_rows <- mat$sweep[
    mat$sweep$gamma == "VFCI (rank-1)" & mat$sweep$grid == "coarse",
  ]
  bnd <- vf_rows[vf_rows$all_bounded & vf_rows$tau > 0, ]
  width_path <- paste(
    sprintf(
      "%s at tau = %.3f",
      formatC(round(bnd$total_width), format = "d", big.mark = ","), bnd$tau
    ),
    collapse = ", "
  )
  rec_all <- c(
    mat$sweep$recession_normalized[mat$sweep$gamma == "VFCI (rank-1)"],
    fac$sweep$recession_normalized[fac$sweep$gamma == "VFCI (rank-1)"]
  )
  rec_max_num <- max(abs(rec_all), na.rm = TRUE)
  rec_max <- formatC(rec_max_num, format = "e", digits = 1)
  rec_pct <- formatC(rec_max_num * 100, format = "g", digits = 2)
  neg_tau <- vf_rows$tau[which(vf_rows$recession_normalized < 0)[1]]
  cross_lines <- if (is.na(neg_tau)) {
    "  the constraint scale. It never crosses zero on the grid."
  } else {
    pos_tau <- vf_rows$tau[which(vf_rows$recession_normalized > 0 &
      vf_rows$tau < neg_tau)]
    c(
      sprintf(
        "  the constraint scale. Its sign flips only between tau = %.3f and",
        max(pos_tau)
      ),
      sprintf(
        "  %.3f (maturities mode), well past the empirical tau* (%s).",
        neg_tau, .fmt_tau(vf_m$tau_star)
      )
    )
  }

  title <- if (is.finite(ratio_m)) {
    c(
      "IDENTIFICATION STRENGTH VIA TAU*: OPTIMIZING GAMMA EXTENDS SLACK",
      sprintf(
        "TOLERANCE ~%.0f-FOLD (TAU* %s -> %s, MATURITIES MODE)",
        ratio_m, .fmt_tau(vf_m$tau_star), .fmt_tau(op_m$tau_star, 3)
      )
    )
  } else {
    c(
      "IDENTIFICATION STRENGTH VIA TAU*: THE VFCI BASELINE TOLERATES",
      sprintf(
        "ALMOST NO SLACK (TAU* = %s, MATURITIES MODE)", .fmt_tau(vf_m$tau_star)
      )
    )
  }

  c(
    title,
    strrep("=", max(nchar(title))),
    "",
    paste("Generated:", as.character(Sys.time())),
    "",
    "BOTTOM LINE",
    "  - VFCI baseline (rank-1): the identified set stays bounded only below",
    sprintf(
      "    tau* ~= %s (maturities mode); beyond tau* it is certified",
      .fmt_tau(vf_m$tau_star, 5)
    ),
    "    unbounded (some direction of theta is unrestricted).",
    "  - Even below tau*, the set is enormous: its total width (summed across",
    "    components of theta) grows from 0 (point identification at tau = 0)",
    sprintf("    to %s.", width_path),
    "    Bounded does not mean informative here.",
    "  - Curvature diagnostic: at every coarse-grid tau tested, the normalized",
    sprintf(
      "    recession metric stays within %s of zero -- a curvature margin of",
      rec_max
    ),
    sprintf(
      "    at most %s%% of the typical constraint scale. The rank-1 baseline",
      rec_pct
    ),
    "    sits essentially on the boundedness knife edge throughout.",
    sprintf(
      "  - Optimized gamma: tau* = %s (maturities) / %s (factors) -- the",
      .fmt_tau_star(op_m$tau_star, op_m$capped),
      .fmt_tau_star(op_f$tau_star, op_f$capped)
    ),
    "    optimizer buys a dramatically larger slack tolerance.",
    sprintf(
      "  - Reduced-form gamma (rank-3, factors mode): tau* = %s vs %s for VFCI",
      .fmt_tau_star(rf_f$tau_star, rf_f$capped), .fmt_tau(vf_f$tau_star)
    ),
    "    -- higher rank alone does not fix the fragility.",
    "",
    "WHAT TAU IS (REFRESHER)",
    "  The identifying restriction is E[eps1*eps2 | Z] = tau * Var(eps2 | Z).",
    "  tau = 0 imposes exact conditional uncorrelatedness of the structural",
    "  shocks given the heteroskedasticity instruments (point identification);",
    "  tau > 0 allows proportional slack, relaxing point to set",
    "  identification. A common tau is applied across all moment conditions.",
    "",
    "WHAT TAU* MEASURES",
    "  tau* is the largest slack at which the identified set for theta is",
    "  still bounded; for tau >= tau* some direction of theta is",
    "  unrestricted. tau* is scale-free, so it summarizes identification",
    "  strength without committing to any particular tau.",
    "",
    "HOW TAU* IS LOCATED",
    "  A profile bound is the smallest or largest value one component of",
    "  theta attains over the identified set; the total width sums (upper -",
    "  lower) across components. Fixed gammas (VFCI, reduced-form): sweep tau",
    "  over a coarse grid plus a fine grid inside the bounded region;",
    "  classify each tau as bounded / unbounded / unreliable (unreliable =",
    "  the profile-bound solver failed its feasibility certificate, so that",
    "  tau is neither finite evidence nor proof of unboundedness); bisect the",
    "  bounded -> unbounded transition. Unreliable taus cluster right at the",
    "  transition, so tau* is bracketed by the certified rows on each side",
    "  (brackets reported under RESULTS BY MODE). Optimized gamma:",
    "  re-optimize gamma at each candidate tau and bisect on whether a",
    "  bounded, valid set is attainable.",
    "",
    "RESULTS BY MODE",
    .mode_block(mat, "MATURITIES MODE (components are bond maturities):"),
    .mode_block(fac, "FACTORS MODE (components are yield-curve factors):"),
    "",
    "CURVATURE DIAGNOSTIC (HONEST READING)",
    "  For each tau we compute the smallest, over unit directions d, of the",
    "  largest curvature max_i d'A_i(tau)d, normalized by the mean Frobenius",
    "  norm of the A_i. A clearly negative value would certify a direction",
    "  along which no constraint curves upward -- a necessary condition for",
    "  an unbounded set. For the VFCI gamma the margin is negligible at",
    sprintf(
      "  every coarse-grid tau tested: |metric| <= %s, at most %s%% of",
      rec_max, rec_pct
    ),
    cross_lines,
    "  It therefore cannot locate tau* and is not a second tau* estimate.",
    "  What it supports is degeneracy: the rank-1 set is borderline-unbounded",
    "  at every tau tested, consistent with the tiny empirical tau* and the",
    "  explosive widths just below it.",
    "",
    "RELATED ARTIFACTS (this directory; {mode} = maturities or factors)",
    "  tau_star_comparison_{mode}.csv ......... tau* per gamma choice",
    "  tau_star_comparison_{mode}.png/.svg .... fixed-gamma width sweeps;",
    "                                           verticals mark every tau*",
    "  tau_star_vfci_blowup_{mode}.png/.svg ... VFCI blow-up near tau*",
    "  tau_star_width_sweep_{mode}.csv ........ the sweep behind the figures",
    "  Raw machine-readable sweeps and settings:",
    "  scripts/output/temp/identification_optimized/tau_star_sweep_{mode}.csv",
    "  and tau_star_comparison_{mode}.rds.",
    "",
    "NUMERICAL NOTES",
    sprintf(
      "  Coarse grid: tau in [%g, %g], step %g; fine grid: up to %d extra",
      min(mat$settings$coarse_taus), max(mat$settings$coarse_taus),
      mat$settings$coarse_taus[2] - mat$settings$coarse_taus[1],
      mat$settings$fine_n
    ),
    "  points inside the bounded region; bisection evaluations are recorded",
    "  too (see the grid column of the sweep files).",
    sprintf(
      "  Bisection: %d iterations. Curvature scan: %d random unit directions",
      mat$settings$bisect_iters, mat$settings$n_dir
    ),
    sprintf(
      "  plus coordinate axes, fixed seed %d, coarse grid only.",
      mat$settings$recession_seed
    ),
    sprintf(
      "  Optimizer oracle: %d multistarts per tau, bracket from %g, cap %g.",
      mat$settings$n_starts, mat$settings$opt_tau_lo, mat$settings$opt_tau_cap
    ),
    "  Width = sum of per-component profile-bound widths. The tau = 0 row is",
    "  point identification (width 0 by construction; curvature metric n/a)."
  )
}
