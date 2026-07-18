# Execution controls used directly by estimators and serialized into spec_id.

LOGVAR_SEARCH_CONTROL <- list(
  grid_n = 41L,
  grid_floor = 100L,
  primary_starts_per_side = 3L,
  audit_starts_per_side = 5L,
  cold_start_check = TRUE,
  pcr_mean_tolerance = 1e-8,
  endpoint_agreement_rtol = 1e-4,
  nearest_neighbor_limit = 5000L,
  start_separation_fraction = 0.05,
  nesting_rtol = 1e-6,
  scan_chunk_size = 5000L,
  cold_start_rtol_fallback = 1e-8,
  point_containment_rtol = 1e-6,
  fitted_lad_grid_n = 31L,
  fitted_lad_grid_cap = 3000L,
  fitted_vol_starts_per_side = 1L,
  logols_full_grid_safety_cap = 1e6
)

LOGVAR_LOGOLS_CONTROL <- list(
  estimator_version = "logols-v1",
  cold_start_rtol = 1e-8
)

LOGVAR_PPML_CONTROL <- list(
  estimator_version = "ppml-v1",
  glm_epsilon = 1e-10,
  glm_maxit = 100L,
  score_tol = 1e-8,
  rank_tol = 1e-10,
  rcond_tol = 1e-10,
  jacobian_rcond_tol = 1e-10,
  cold_start_rtol = 1e-6,
  boundary_switch = TRUE,
  finite_mean_switch = TRUE,
  rank_switch = TRUE,
  cold_switch = TRUE,
  fallback_order = paste(
    "supplied_start",
    "fallback_starts",
    "intercept_only",
    "glm_default",
    sep = ","
  ),
  morton_bits = 17L,
  exact_double_bits = 53L,
  pilot_overflow_margin = 5,
  pilot_condition_limit = 1e10,
  pilot_grid_points = 10L
)

LOGVAR_HARVEY_CONTROL <- list(
  estimator_version = "harvey-v1",
  score_tol = 1e-8,
  response_rank_tol = 1e-8,
  recession_rank_tol = 1e-10,
  rcond_tol = 1e-10,
  jacobian_rcond_tol = 1e-10,
  newton_rcond_tol = 1e-12,
  recession_rate_multiplier = 1e-9,
  certificate_tol = 1e-8,
  line_search_halvings = 30L,
  q_noise_multiplier = 4,
  score_progress_multiplier = 10,
  maxit = 1000L,
  rel_change_tol = 1e-10,
  lp_xtol_rel = 1e-12,
  lp_maxeval = 2000L,
  lp_bound = 1e6,
  cold_start_rtol = 1e-6,
  fit_stage_policy = c("warm", "ppml_at_b", "standalone"),
  standalone_start_policy = c(
    "ppml_point",
    "logols_shifted",
    "intercept_only"
  ),
  scaling_policy = "none"
)

LOGVAR_LAD_CONTROL <- list(
  estimator_version = "lad-v1",
  grid_cap = 20000L,
  fit_budget = 45000L,
  phase_caps = c(
    scan = 20000L,
    probe = 16000L,
    nonunique = 2000L,
    polish = 26000L,
    cold_start = 4000L
  ),
  objective_rtol = 1e-8,
  coefficient_rtol = 1e-4,
  fn_epsilon = 1e-6,
  guard_ratio = 1e-12,
  witness_hyperplane_tol = 1e-13,
  witness_constraint_tol = 1e-8,
  witness_maxeval = 2000L,
  anchor_maxeval = 1000L,
  anchor_radius = 0.05,
  probe_ratio = 0.5,
  probe_max_steps = 40L,
  tail_stable_window = 6L,
  tail_divergent_window = 8L,
  tail_increment_count = 4L,
  tail_stability_rtol = 1e-6,
  tail_slope_floor = 1e-12,
  tail_slope_agreement_rtol = 0.20,
  tail_move_tolerance_multiplier = 10,
  tail_relative_move_floor = 0.05,
  tail_min_m_span = 12,
  tail_min_r_squared = 0.98,
  witness_edge_fractions = c(-0.5, 0.5),
  witness_bisection_steps = 50L,
  witness_simultaneous_cap = 8L,
  witness_cone_step_fraction = 0.02,
  cold_start_rtol = 1e-9
)

LOGVAR_LAD_OFFLINE_REFINEMENT_CONTROL <- list(
  refine_radii = c(0.05, 0.0125)
)

stopifnot(
  LOGVAR_SEARCH_CONTROL$grid_n >= 2L,
  LOGVAR_SEARCH_CONTROL$grid_floor >= 1L,
  LOGVAR_SEARCH_CONTROL$fitted_lad_grid_n >= 2L,
  LOGVAR_SEARCH_CONTROL$fitted_lad_grid_cap >= 1L,
  LOGVAR_SEARCH_CONTROL$fitted_vol_starts_per_side >= 1L,
  LOGVAR_SEARCH_CONTROL$logols_full_grid_safety_cap >= 1,
  LOGVAR_LOGOLS_CONTROL$cold_start_rtol > 0,
  LOGVAR_PPML_CONTROL$glm_epsilon > 0,
  LOGVAR_HARVEY_CONTROL$line_search_halvings >= 0L,
  sum(LOGVAR_LAD_CONTROL$phase_caps) >=
    LOGVAR_LAD_CONTROL$fit_budget
)
