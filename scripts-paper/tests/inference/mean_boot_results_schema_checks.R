# Exact legacy public schema, kept separate to preserve the strict test-file budget.

check(
  "mean_boot_results has the exact legacy public field order",
  identical(
    names(mbr_out),
    c(
      "b_reps", "block", "seed", "inference_contract", "point_se",
      "point_ci", "point_band", "tau_star_band", "tau_star_share_bounded",
      "inference", "provenance", "point_draws", "n_point_deficient",
      "endpoint_draws", "tau_star_draws", "n_capped", "n_failed",
      "failure_causes"
    )
  )
)
