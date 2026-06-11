# Post-selection report caveats and simulation section. Split out of
# postsel_report_text.R to keep both files well under the 200-line
# rule; sourced by the report driver and the text tests alongside it.

postsel_caveat_lines <- function(settings) {
  c(
    "Caveats (read before quoting any number above):",
    paste0(
      "- Half-sample widths are mechanically wider: block",
      " evaluations use roughly half the observations. The honest",
      " comparison for the split arm is the fixed-weights set",
      " evaluated on the SAME block (fixed_e), never a full-sample",
      " width."
    ),
    paste0(
      "- Widths are never comparable across different evaluation",
      " windows or constraint counts; this report never pools them."
    ),
    paste0(
      "- Serial dependence across the block boundary makes 'weights",
      " fixed relative to the evaluation sample' approximate; the ",
      settings$gap, "-quarter gap attenuates but does not remove it."
    ),
    paste0(
      "- The spec's fixed-Lambda inclusion (theta_0 in the set for",
      " any fixed, ex ante admissible Lambda) transfers to the split",
      " arm only if the maintained bound holds AT the selected",
      " weights -- a population property the data cannot certify."
    ),
    paste0(
      "- Stationarity across blocks is maintained (spec assumption:",
      " moments do not depend on t); see the stage-01 time-series",
      " properties output."
    ),
    paste0(
      "- The full-sample optimized arm remains a width-minimizing",
      " computational benchmark, not a confidence statement. Uniform",
      " post-selection inference over the admissibility class is out",
      " of scope (plan decision log)."
    ),
    ""
  )
}

postsel_sim_lines <- function(sim, acceptance) {
  if (is.null(sim)) {
    return(c(
      "Simulation validation: NOT FOUND.",
      paste0(
        "No simulation artifacts present. The split rows above are",
        " solver outputs only; the selection-honest interpretation",
        " is NOT validated in this artifact set. Run",
        " scripts/post_selection/split_simulation.R."
      ),
      ""
    ))
  }
  cov <- aggregate_sim_coverage(sim$results)
  header <- sprintf(
    paste0(
      "Simulation validation (%s; T = %d, tau = %.3f, phi = %.2f,",
      " reps = %d):"
    ),
    if (isTRUE(sim$settings$quick)) {
      "QUICK SMOKE GRID"
    } else {
      "full grid"
    },
    sim$settings$t_obs, sim$settings$tau, sim$settings$phi,
    sim$settings$reps
  )
  tab <- c(
    sprintf(
      "  %-4s %-12s %-9s %-7s %-5s", "K", "arm", "coverage", "se",
      "n"
    ),
    vapply(seq_len(nrow(cov)), function(k) {
      sprintf(
        "  %-4d %-12s %-9.3f %-7.3f %-5d",
        cov$k_inst[k], cov$arm[k], cov$covered[k], cov$se[k],
        cov$n[k]
      )
    }, character(1))
  )
  verdict <- if (isTRUE(sim$settings$quick)) {
    paste0(
      "Acceptance: NOT EVALUATED on the smoke grid (too few",
      " replications). The selection-honest interpretation is NOT",
      " validated by this artifact; run the full simulation."
    )
  } else {
    checks <- acceptance$checks
    c(
      paste0(
        "Acceptance (pre-specified margins; a FAIL is reported,",
        " never softened):"
      ),
      vapply(names(checks), function(nm) {
        sprintf(
          "  %-38s %s", nm,
          if (isTRUE(checks[[nm]])) "PASS" else "FAIL"
        )
      }, character(1)),
      if (all(unlist(checks))) {
        paste0(
          "Verdict: PASS -- on the synthetic DGP, full-sample",
          " selection degrades coverage of theta0 and the split",
          " restores it to the fixed-weights benchmark."
        )
      } else {
        paste0(
          "Verdict: FAIL -- the split procedure did NOT deliver its",
          " promised coverage behavior on the synthetic DGP. Do not",
          " quote the split rows as selection-honest; per the plan's",
          " honesty-precedence rule, report this and stop."
        )
      }
    )
  }
  c(header, tab, verdict, "")
}
