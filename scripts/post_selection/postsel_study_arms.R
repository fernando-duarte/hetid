# Arm grid for the post-selection split study: which weights are
# evaluated on which window. fixed_* rows use the ex ante VFCI
# weights; optimized rows use the weights selected on the sel
# window, evaluated on the eval window. split_primary is the
# feature; the rest are benchmarks and diagnostics (quoting
# whichever direction looks better would reintroduce selection --
# the primary direction is fixed ex ante: select on early, evaluate
# on late).

postsel_arm_specs <- function() {
  list(
    list(
      arm = "fixed_full", weights = "vfci_fixed",
      sel = "none", eval = "full"
    ),
    list(
      arm = "fixed_s", weights = "vfci_fixed",
      sel = "none", eval = "s"
    ),
    list(
      arm = "fixed_e", weights = "vfci_fixed",
      sel = "none", eval = "e"
    ),
    list(
      arm = "opt_full_insample", weights = "optimized",
      sel = "full", eval = "full"
    ),
    list(
      arm = "split_primary", weights = "optimized",
      sel = "s", eval = "e"
    ),
    list(
      arm = "split_reverse", weights = "optimized",
      sel = "e", eval = "s"
    ),
    list(
      arm = "insample_s", weights = "optimized",
      sel = "s", eval = "s"
    ),
    list(
      arm = "insample_e", weights = "optimized",
      sel = "e", eval = "e"
    )
  )
}

# Evaluate every arm and assemble the long-format grid. selection is
# the named list of run_lambda_optimization results per window;
# windows the named list of block_moments results.
evaluate_arm_grid <- function(arm_specs, gamma_baseline, selection,
                              windows, tau, component_labels) {
  evaluations <- lapply(arm_specs, function(spec) {
    lam <- if (identical(spec$weights, "vfci_fixed")) {
      gamma_baseline
    } else {
      selection[[spec$sel]]$lambda_optimized
    }
    evaluate_lambda_set(lam, tau, windows[[spec$eval]]$moments)
  })
  names(evaluations) <- vapply(arm_specs, `[[`, character(1), "arm")
  grid <- do.call(rbind, lapply(seq_along(arm_specs), function(k) {
    spec <- arm_specs[[k]]
    arm_rows(
      spec$arm, spec$weights, spec$sel, spec$eval,
      windows[[spec$eval]]$n_obs, evaluations[[k]],
      component_labels
    )
  }))
  list(evaluations = evaluations, grid = grid)
}
