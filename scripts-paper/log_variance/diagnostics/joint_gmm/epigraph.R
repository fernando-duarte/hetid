# Pinned numerical constants, epigraph initialization, and the constructive
# search/box/floor statuses for the joint log-variance GMM (joint-GMM,
# logvar-joint-gmm, dossier sections 3.3, 5, 6). The constants block is the
# single source of the four separated controls, the box-width schedule, the
# grid/candidate/pattern budgets, and the Option-B dimension counts; the
# substantive moment_delta tolerance is deliberately not one of them. The
# statuses are evidence-calibrated: absence never certifies emptiness, an
# expanding box that keeps moving is unreliable, unbounded needs a verified
# direction certificate, and a floor-sensitive endpoint is never reported
# bounded. The bespoke SLSQP epigraph solver lives in the sourced companion so
# this file stays under the line cap. Definitions only; sourced by
# the joint-GMM test entrypoint and the search/projection driver.

# Frozen numerical controls and dimensions. The four solver/verification
# tolerances are separately named with no ordering implied; moment_delta is a
# substantive tolerance owned elsewhere and never appears here.
logvar_joint_gmm_constants <- list(
  param_xtol_rel = 1e-9,
  objective_tol = 1e-10,
  constraint_tol = 1e-8,
  root_tol = 1e-8,
  box_half_widths = c(4, 8, 16),
  pattern_start_cap = 256L,
  gmm_grid_cap = 4000L,
  candidate_eval_cap = 60000L,
  grid_n = 41L,
  grid_floor = 100L,
  per_solve_maxeval = 1000L,
  n_moments_unprofiled = 10L,
  n_parameters_unprofiled = 9L,
  n_moments_profiled = 8L,
  n_parameters_profiled = 7L
)

# Fresh epigraph initialization from the scaled moment vector at the start head.
# r0 is the attained scaled sup-norm; an exact zero short-circuits to the global
# nonnegative value zero, otherwise r_start slacks r0 by a relative step so the
# epigraph inequalities start strictly interior.
logvar_joint_epigraph_start <- function(scaled_g0) {
  r0 <- max(abs(scaled_g0))
  if (r0 == 0) {
    return(list(r0 = 0, r_start = 0, short_circuit = TRUE))
  }
  list(r0 = r0, r_start = r0 + max(1e-12, 1e-6 * r0), short_circuit = FALSE)
}

# Constructive search status. Incomplete coverage or a material failure is
# unreliable; otherwise a within-root_tol candidate is a nonemptiness witness
# and its absence is only not_found_within_search, never an emptiness claim.
logvar_joint_classify_search <- function(attained, has_root_tol_candidate,
                                         coverage_complete, material_failure) {
  if (material_failure || !coverage_complete) {
    "unreliable"
  } else if (has_root_tol_candidate) {
    "found_within_root_tol"
  } else {
    "not_found_within_search"
  }
}

# A candidate whose scaled sup-norm sits within moment_delta certifies the delta
# region is nonempty; the test nests in delta because a smaller sup-norm stays
# feasible at any larger tolerance.
logvar_joint_delta_nonempty <- function(scaled_linf, moment_delta) {
  scaled_linf <= moment_delta
}

# Nested-tau floor: keep the current minimum only when it strictly improves on
# the prior one past tol, otherwise carry the prior minimum and its arg-min so a
# supposedly larger set never reports a worse attained value.
logvar_joint_carry_floor <- function(prev_min, prev_argmin, cur_min, cur_argmin,
                                     tol = 1e-12) {
  if (cur_min < prev_min - tol) {
    list(min = cur_min, argmin = cur_argmin, floored = FALSE)
  } else {
    list(min = prev_min, argmin = prev_argmin, floored = TRUE)
  }
}

# Expanding-box verdict. Interior attainment agreeing across two widths is
# bounded; persistent movement is unbounded only with a verified direction
# certificate, and everything else is unreliable.
logvar_joint_box_status <- function(interior, two_width_agree, box_active,
                                    moving, direction_certificate = NULL) {
  if (interior && two_width_agree && !box_active && !moving) {
    "bounded"
  } else if (moving && isTRUE(direction_certificate$certified)) {
    "unbounded"
  } else {
    "unreliable"
  }
}

# Floor-sensitivity audit over the replayed floor multipliers: stable only when
# every status matches and the values agree within a relative tolerance, in
# which case the shared status stands, otherwise the result is unreliable.
logvar_joint_floor_audit <- function(values, statuses, tol_factor = 1e-5) {
  same_status <- length(unique(statuses)) == 1L
  within <- all(
    abs(values[-1L] - values[1L]) <= tol_factor * max(1, abs(values[1L]))
  )
  if (same_status && within) {
    list(stable = TRUE, status = statuses[[1L]])
  } else {
    list(stable = FALSE, status = "unreliable")
  }
}

# The bespoke SLSQP epigraph solver and the direction-certificate hook live in
# their own module so this file stays under the repository line cap; sourced
# here so callers see the same definitions.
source(paper_path("log_variance", "diagnostics", "joint_gmm", "epigraph_solver.R"))
