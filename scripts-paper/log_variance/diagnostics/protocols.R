# Immutable runtime protocols for result-bearing log-variance diagnostics.

LOGVAR_DYNAMICS_GATE_PROTOCOL <- list(
  version = "1.0.0",
  tested_lags = c(1L, 4L, 8L),
  gate_lag = 4L,
  alpha = 0.05,
  acf_max = 8L,
  tie_tol = 1e-10,
  verdicts = c("reject", "non_reject", "unreliable")
)

LOGVAR_JOINT_NULL_CONTROL <- list(
  version = "1.0.0",
  grid_n = 13L,
  grid_floor = 100L,
  root_tol = 1e-6,
  feasibility_tol = 1e-4,
  crossing_rel_tol = 1e-6,
  machine_adjacent_rel_tol = 1e-8,
  coordinate_agreement_rel_tol = 1e-6,
  fresh_map_rel_tol = 1e-10,
  box_active_rel_tol = 1e-6,
  required_agreeing_starts = 2L,
  separated_starts = 5L,
  separated_start_fraction = 0.05,
  perturbation_fractions = c(0.01, 0.03, 0.10),
  perturbation_rank_tol = 1e-10,
  perturbation_dedupe_tol = 1e-12,
  perturbation_sign_tol = 1e-14,
  puncture_ratio = 0.15,
  off_manifold_root_multiplier = 100,
  linf_gap_root_multiplier = 10,
  linf_gap_rel_tol = 0.10,
  epigraph_xtol_rel = 1e-10,
  epigraph_maxeval = 500L,
  epigraph_bound = 1e6,
  epigraph_constraint_tol = 1e-8
)

stopifnot(
  LOGVAR_DYNAMICS_GATE_PROTOCOL$gate_lag %in%
    LOGVAR_DYNAMICS_GATE_PROTOCOL$tested_lags,
  LOGVAR_JOINT_NULL_CONTROL$root_tol > 0,
  LOGVAR_JOINT_NULL_CONTROL$feasibility_tol > 0
)
