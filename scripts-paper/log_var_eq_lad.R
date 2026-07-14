# The median (LAD) log-variance estimator: the b-indexed map theta_hat^0.5(b)
# whose inner fit is the conditional-median regression of z(b) = 2 log|w1 - W2 b|
# on (1, PC_R) via quantreg::rq.fit(method = "br"). Holds the engine-config
# constants, the single-sourced normalization constants, the canonical spec_id,
# the augmented-context helper the domain functions read, and the Plan 7
# estimator object. The br response fit, nonuniqueness probe, scale reference and
# domain guard live in log_var_eq_lad_fit.R; the witnesses, anchors and M-scale
# probes in log_var_eq_lad_domain.R; both consumed at call time, never sourced
# here so this file loads before either exists. The analyze_domain hooks live in
# log_var_eq_lad_hooks.R (sourced below, a same-author sibling under the line
# cap). Definitions only; sourced after log_var_eq_map.R and the engine.
source("scripts-paper/log_var_eq_lad_hooks.R")

# Engine configuration (Plan 3 design decision five): the lattice traversal
# removes the O(m^2) nearest-neighbor ordering, so the dense-grid recommendation
# binds and the grid cap rises to 20000 within a 40000 per-tau fit budget. Phase
# caps reserve scan/probe/refinement and a combined polish/nonunique/cold pool
# (nonunique <= 2000); the global budget bounds the total.
logvar_lad_grid_cap <- 20000L
logvar_lad_fit_budget <- 40000L
logvar_lad_phase_caps <- c(
  scan = 20000L, probe = 10000L, refinement = 6000L,
  nonunique = 2000L, polish = 4000L, cold_start = 4000L
)

# The numerical guard ratio logvar_lad_guard_ratio (= 1e-12) is single-sourced in
# the fit module (log_var_eq_lad_fit.R), which applies it in the domain split; the
# spec_id and the claim rule read that one definition. The fit module is a hard
# dependency of this estimator (logvar_lad_fit / logvar_lad_scale_reference), so it
# is always loaded before the estimator is constructed or a hook runs.

# The deterministic second-pass refinement radii (times the frozen b_scales):
# tensor offsets {-r, 0, r}^K around every arg-extremum and verified witness.
# Defined here with the other constants; the schedule in log_var_eq_lad_refine.R
# consumes it, and it enters spec_id so a radius change re-stamps the fit.
logvar_lad_refine_radii <- c(0.05, 0.0125)

# Median normalization constants, single-sourced as exact expressions and used
# only as interpretive checks (never imposed on a fit). Under normality
# log eps^2 = x' theta_var + log chi^2_1, so the LAD intercept sits at the
# variance intercept plus log median(chi^2_1) and above the mean-log intercept by
# the difference against E[log chi^2_1] = digamma(1/2) + log 2.
logvar_lad_median_chisq1 <- stats::qchisq(0.5, 1)
logvar_lad_median_lnchisq_gap <- log(stats::qchisq(0.5, 1))
logvar_lad_median_meanlog_gap <-
  log(stats::qchisq(0.5, 1)) - (digamma(0.5) + log(2))

# Canonical fit-identity string: bytewise-sorted key=value records over every
# fit-changing field, numerics at 17 digits so the stamp is deterministic and
# independent of options(digits); vectors join with ",". Mirrors the PPML and
# Harvey renderers but kept local so this module needs neither sourced.
logvar_lad_spec_id <- function(fields) {
  render <- function(x) {
    if (is.numeric(x)) {
      x <- formatC(x, digits = 17, format = "fg", flag = "#")
    } else {
      x <- as.character(x)
    }
    paste(x, collapse = ",")
  }
  records <- vapply(
    names(fields),
    function(k) paste0(k, "=", render(fields[[k]])),
    character(1)
  )
  paste(sort(records, method = "radix"), collapse = "\n")
}

# The augmented context the domain functions read: the engine's hook context
# plus the geometry members (w1, w2, x_mat, e_scale_ref) the witness, anchor and
# probe solves need. The engine hands analyze_domain only the generic seam, so
# the hooks add geometry here before calling into the domain module.
logvar_lad_augment_ctx <- function(ctx, w1, w2, x_mat, e_scale_ref) {
  ctx$w1 <- w1
  ctx$w2 <- w2
  ctx$x_mat <- x_mat
  ctx$e_scale_ref <- e_scale_ref
  ctx
}

# The Plan 7 estimator object. Freezes X = cbind(1, PC_R) and the b-independent
# residual scale once: e_ref defaults to the naive news OLS residual (a positive
# fallback for the 4-argument test calls), but the driver passes the mean
# equation's naive residual so e_scale_ref equals the pipeline median(abs(eps)).
# fit_at_b delegates to logvar_lad_fit (br, no warm start); analyze_domain wires
# the census/witness precheck, the probe-protocol sides, and the exact/guarded
# claim rule. No jacobian_at_b (nonsmooth -> the engine polishes with COBYLA),
# no scan_grid, and no start bundle (br exposes no warm start and no later plan
# starts from a median fit).
logvar_lad_estimator <- function(w1, w2, pcr, qtr, e_ref = NULL) {
  x_mat <- cbind(1, pcr)
  colnames(x_mat) <- c("(Intercept)", colnames(pcr))
  if (is.null(e_ref)) {
    e_ref <- stats::lm.fit(cbind(1, w2), w1)$residuals
  }
  e_scale_ref <- logvar_lad_scale_reference(e_ref)
  # per-estimator scratch the precheck fills each tau (the cached evaluator and
  # whether the box has empty interior); coef_objective reads it to pin the
  # nonsmooth COBYLA polish on a degenerate set instead of exploiting the slack.
  geom <- new.env(parent = emptyenv())
  geom$evaluate_fit <- NULL
  geom$degenerate <- FALSE
  geom$center <- NULL
  spec_id <- logvar_lad_spec_id(list(
    estimator_version = "lad-v1",
    quantreg_version = as.character(utils::packageVersion("quantreg")),
    inner_method = "br", nonunique_method = "fn",
    obj_equal_rtol = 1e-8, coef_diff_rtol = 1e-4,
    guard_ratio = logvar_lad_guard_ratio,
    scale_source = "median_abs_naive_ols_resid_positive_fallback",
    witness_hyperplane_tol = 1e-13, witness_constraint_tol = 1e-8,
    witness_maxeval = 2000L, anchor_maxeval = 1000L, anchor_radius = 0.05,
    probe_ratio = 0.5, probe_max_steps = 40L,
    refine_radii = logvar_lad_refine_radii, refine_order = "type,row,radius,offset",
    cold_start_rtol = 1e-9, grid_cap = logvar_lad_grid_cap,
    fit_budget = logvar_lad_fit_budget,
    scan_cap = unname(logvar_lad_phase_caps[["scan"]]),
    probe_cap = unname(logvar_lad_phase_caps[["probe"]]),
    refinement_cap = unname(logvar_lad_phase_caps[["refinement"]]),
    nonunique_cap = unname(logvar_lad_phase_caps[["nonunique"]]),
    polish_cap = unname(logvar_lad_phase_caps[["polish"]]),
    cold_start_cap = unname(logvar_lad_phase_caps[["cold_start"]])
  ))
  metadata <- list(
    estimator = "lad", target_functional = "theta_median",
    intercept_normalization = paste0(
      "conditional median of log eps^2 (benchmark mean-log + 0.4828 under ",
      "normality; absorbs 2 log|m_0|)"
    ),
    sample_id = logvar_sample_id(qtr, w1, w2, pcr), smoothness = "nonsmooth",
    inner_solver = "quantreg rq.fit br", response_scale = "log",
    response_scale_value = 1,
    scale_reference = "naive OLS absolute-residual scale with positive fallback",
    spec_id = spec_id, cold_start_rtol = 1e-9, traversal = "lattice"
  )
  list(
    metadata = metadata, coef_labels = colnames(x_mat), start_bundle = NULL,
    fit_at_b = function(b, start = NULL) {
      logvar_lad_fit(b, w1, w2, x_mat, e_scale_ref)
    },
    # NULL on a full-dimensional box -> the engine's default budgeted objective
    # runs unchanged; a degenerate box gets the center-pinned objective so the
    # derivative-free polish cannot widen a measure-zero set through the 1e-4
    # feasibility slack. Never affects production sets, which are always fat.
    coef_objective = logvar_lad_coef_objective(geom),
    analyze_domain = list(
      precheck = logvar_lad_precheck_hook(w1, w2, x_mat, e_scale_ref, geom),
      sides = logvar_lad_sides_hook(w1, w2, x_mat, e_scale_ref),
      claim_failure = logvar_lad_claim_hook(w1, w2, e_scale_ref)
    )
  )
}
