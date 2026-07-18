# The median (LAD) log-variance estimator: the b-indexed map theta_hat^0.5(b)
# whose inner fit is the conditional-median regression of z(b) = 2 log|w1 - W2 b|
# on (1, PC_R) via quantreg::rq.fit(method = "br"). Holds the engine-config
# constants, the single-sourced normalization constants, the canonical spec_id,
# the augmented-context helper the domain functions read, and the estimator-engine
# estimator object. The br response fit, nonuniqueness probe, scale reference and
# domain guard live in fit.R; the witnesses, anchors and M-scale
# probes in crossing_domain.R; both consumed at call time, never sourced
# here so this file loads before either exists. The analyze_domain hooks live in
# engine_hooks.R (sourced below, a same-author sibling under the line
# cap). Definitions only; sourced after residual_map.R and the engine.
paper_source_once(paper_path(
  "support", "statistics", "normalizations.R"
))
paper_source_once(paper_path("log_variance", "estimators", "lad", "engine_hooks.R"))

# Median normalization constants are owned by normalizations.R and used only as
# interpretive checks (never imposed on a fit). Under normality,
# log eps^2 = x' theta_var + log chi^2_1, so the LAD intercept sits at the
# variance intercept plus log median(chi^2_1) and above the mean-log intercept by
# the difference against E[log chi^2_1] = digamma(1/2) + log 2.

# The augmented context the domain functions read: the engine's hook context
# plus the geometry members (w1, w2, x_mat, e_scale_ref) the witness, anchor and
# probe solves need. The engine hands analyze_domain only the generic seam, so
# the hooks add geometry here before calling into the domain module.
logvar_lad_augment_ctx <- function(
  ctx,
  w1,
  w2,
  x_mat,
  e_scale_ref,
  control
) {
  ctx$w1 <- w1
  ctx$w2 <- w2
  ctx$x_mat <- x_mat
  ctx$e_scale_ref <- e_scale_ref
  ctx$control <- control
  ctx
}

# The estimator-engine estimator object. Freezes X = cbind(1, PC_R) and the b-independent
# residual scale once: e_ref defaults to the naive news OLS residual (a positive
# fallback for the 4-argument test calls), but the driver passes the mean
# equation's naive residual so e_scale_ref equals the pipeline median(abs(eps)).
# fit_at_b delegates to logvar_lad_fit (br, no warm start); analyze_domain wires
# the census/witness precheck, the probe-protocol sides, and the exact/guarded
# claim rule. No jacobian_at_b (nonsmooth -> the engine polishes with COBYLA),
# no scan_grid, and no start bundle (br exposes no warm start and no downstream
# starts from a median fit).
logvar_lad_estimator <- function(
  w1,
  w2,
  pcr,
  qtr,
  e_ref = NULL,
  control = LOGVAR_LAD_CONTROL
) {
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
  spec_id <- logvar_spec_id(c(
    logvar_flatten_spec(control, "control"),
    list(
      quantreg_version =
        as.character(utils::packageVersion("quantreg")),
      inner_method = "br",
      nonunique_method = "fn",
      scale_source =
        "median_abs_naive_ols_resid_positive_fallback",
      refine_order = "type,row,radius,offset"
    )
  ))
  metadata <- list(
    estimator = "lad", target_functional = "theta_median",
    intercept_normalization = sprintf(
      paste0(
        "conditional median of log eps^2 (benchmark mean-log + %s under ",
        "normality; absorbs 2 log|m_0|)"
      ),
      format(LOGVAR_NORMAL_MEDIAN_MEANLOG_GAP, digits = 4L)
    ),
    sample_id = logvar_sample_id(qtr, w1, w2, pcr), smoothness = "nonsmooth",
    inner_solver = "quantreg rq.fit br", response_scale = "log",
    response_scale_value = 1,
    scale_reference = "naive OLS absolute-residual scale with positive fallback",
    spec_id = spec_id,
    fit_control = control,
    cold_start_rtol = control$cold_start_rtol,
    traversal = "lattice"
  )
  list(
    metadata = metadata, coef_labels = colnames(x_mat), start_bundle = NULL,
    fit_at_b = function(b, start = NULL) {
      logvar_lad_fit(
        b,
        w1,
        w2,
        x_mat,
        e_scale_ref,
        control
      )
    },
    # NULL on a full-dimensional box -> the engine's default budgeted objective
    # runs unchanged; a degenerate box gets the center-pinned objective so the
    # derivative-free polish cannot widen a measure-zero set through the 1e-4
    # feasibility slack. Never affects production sets, which are always fat.
    coef_objective = logvar_lad_coef_objective(geom),
    analyze_domain = list(
      precheck = logvar_lad_precheck_hook(
        w1, w2, x_mat, e_scale_ref, geom, control
      ),
      sides = logvar_lad_sides_hook(
        w1, w2, x_mat, e_scale_ref, control
      ),
      claim_failure = logvar_lad_claim_hook(
        w1, w2, e_scale_ref, control
      )
    )
  )
}
