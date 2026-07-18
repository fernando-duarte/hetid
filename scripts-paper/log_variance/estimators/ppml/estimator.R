# The PPML (quasi-Poisson log-link) log-variance estimator: the b-indexed map
# theta_hat_P(b) solving E[R {eps^2 - exp(R' theta)}] = 0 at each Lewbel
# candidate b, packaged for the shared set engine. Holds the b wrapper, the
# analytic implicit Jacobian, the input validator, the canonical spec_id, the
# estimator-engine estimator constructor, and the editorial panel-order rule. The core
# IRLS solve, score/information internals, and start bundle live in the fit
# module sourced below; this layer forms y(b) = (w1 - w2 b)^2 and delegates.
# Definitions only; sourced after the map/engine/log-OLS modules (logvar_sample_id).

paper_source_once(paper_path("log_variance", "estimators", "ppml", "fit.R"))
paper_source_once(paper_path(
  "log_variance", "estimators", "ppml", "input_contract.R"
))

# The b wrapper: form the reference residual and its squared response on the
# original scale (the fit module does the scaling), delegate to the response
# fit, and record the original-scale min|eps| the driver's fragility line reads.
logvar_ppml_fit <- function(b, w1, w2, x_mat, start = NULL,
                            fallback_starts = list(), response_scale = 1,
                            control = LOGVAR_PPML_CONTROL) {
  e <- drop(w1 - w2 %*% b)
  y <- e^2
  fit <- logvar_ppml_fit_response(
    y, x_mat,
    start = start,
    fallback_starts = fallback_starts,
    response_scale = response_scale,
    control = control
  )
  fit$diagnostics$min_abs_eps <- min(abs(e))
  fit
}

# Analytic implicit Jacobian D_b theta_hat_P(b), returned only when the fit is an
# accepted LOGVAR_FIT_STATUS[["ok"]] solution (a bare coefficient vector cannot certify acceptance)
# and the column-normalized information is well conditioned; otherwise NULL so
# the engine's derivative-free path takes over. The scaled-fit coefficients
# (warm_start) supply mu_star; the (-2/s) factor keeps the response derivative on
# the fit's scale, so this is the Jacobian of the original-scale map. Solved by
# explicit Cholesky of the column-normalized D^-1 A D^-1, never an inverse.
logvar_ppml_jacobian <- function(
  fit, b, w1, w2, x_mat, response_scale = 1,
  control = LOGVAR_PPML_CONTROL
) {
  if (!logvar_fit_ok(fit)) {
    return(NULL)
  }
  mu_star <- exp(drop(x_mat %*% fit$warm_start))
  e <- drop(w1 - w2 %*% b)
  a_mat <- crossprod(x_mat, mu_star * x_mat)
  rhs <- crossprod(x_mat, (-2 / response_scale) * e * w2)
  d_scale <- sqrt(colSums(mu_star * x_mat^2))
  if (!all(is.finite(d_scale)) || any(d_scale <= 0)) {
    return(NULL)
  }
  a_s <- a_mat / tcrossprod(d_scale)
  if (!all(is.finite(a_s)) ||
    rcond(a_s) < control$jacobian_rcond_tol) {
    return(NULL)
  }
  r_chol <- tryCatch(chol(a_s), error = function(cond) NULL)
  if (is.null(r_chol)) {
    return(NULL)
  }
  sol_s <- backsolve(r_chol, forwardsolve(t(r_chol), rhs / d_scale))
  out <- sol_s / d_scale
  rownames(out) <- colnames(x_mat)
  out
}

# The estimator-engine estimator object. At construction it fits the scale anchor once
# (failing closed when the anchor response has no positive value) and the Lewbel
# point once when b_point is a finite numeric vector, closing over the single
# typed fallback warm start (point coef_scaled when available, else anchor's).
# Point and anchor identities are never conflated. fit_at_b / jacobian_at_b
# close over the frozen X = cbind(1, pcr); no scan_grid, no analyze_domain.
logvar_ppml_estimator <- function(w1, w2, pcr, qtr, b_point = NULL,
                                  scale_anchor_b, scale_anchor_source,
                                  response_scale = 1,
                                  control = LOGVAR_PPML_CONTROL) {
  x_mat <- logvar_design_matrix(pcr)
  anchor_y <- drop(w1 - w2 %*% scale_anchor_b)^2
  if (!any(anchor_y > 0)) {
    stop("logvar_ppml_estimator: scale anchor response has no positive value")
  }
  anchor_fit <- logvar_ppml_fit(
    scale_anchor_b, w1, w2, x_mat,
    response_scale = response_scale,
    control = control
  )
  scale_anchor_bundle <- logvar_ppml_start_bundle(
    anchor_fit, response_scale, scale_anchor_source, scale_anchor_b
  )
  start_bundle <- NULL
  if (!is.null(b_point) && !anyNA(b_point)) {
    point_fit <- logvar_ppml_fit(
      b_point, w1, w2, x_mat,
      response_scale = response_scale,
      control = control
    )
    start_bundle <- logvar_ppml_start_bundle(point_fit, response_scale, "lewbel_point", b_point)
  }
  fallback <- if (!is.null(start_bundle)) {
    list(start_bundle$coef_scaled)
  } else if (!is.null(scale_anchor_bundle)) {
    list(scale_anchor_bundle$coef_scaled)
  } else {
    list()
  }
  realized_branch <- if (!is.null(start_bundle)) "point" else "anchor"
  realized_b_point <- if (!is.null(b_point) && !anyNA(b_point)) b_point else "null"
  spec_id <- logvar_spec_id(c(
    logvar_flatten_spec(control, "control"),
    list(
      response_scale = response_scale,
      b_point = realized_b_point,
      scale_anchor_b = scale_anchor_b,
      scale_anchor_source = scale_anchor_source,
      realized_branch = realized_branch
    )
  ))
  list(
    metadata = list(
      estimator = "ppml", target_functional = "theta_var",
      intercept_normalization =
        "log conditional variance (absorbs 2 log|m_0|; Jensen gap vs mean-log)",
      sample_id = logvar_sample_id(qtr, w1, w2, pcr), smoothness = "smooth",
      inner_solver = sprintf(
        "%s %s(%s) IRLS",
        control$fit_function,
        control$family,
        control$link
      ),
      response_scale = "variance",
      response_scale_value = response_scale,
      scale_reference = "median positive y at the scale anchor",
      scale_anchor_b = scale_anchor_b, scale_anchor_source = scale_anchor_source,
      spec_id = spec_id,
      fit_control = control,
      cold_start_rtol = control$cold_start_rtol
    ),
    # explicit axis so fail-closed engine results keep the theta_var labels
    coef_labels = colnames(x_mat),
    start_bundle = start_bundle,
    scale_anchor_bundle = scale_anchor_bundle,
    fit_at_b = function(b, start = NULL) {
      logvar_ppml_fit(
        b, w1, w2, x_mat,
        start = start,
        fallback_starts = fallback,
        response_scale = response_scale,
        control = control
      )
    },
    jacobian_at_b = function(b, fit = NULL) {
      if (is.null(fit)) {
        return(NULL)
      }
      logvar_ppml_jacobian(
        fit, b, w1, w2, x_mat, response_scale, control
      )
    }
  )
}

# Editorial panel-order rule, keyed to the benchmark crossing count at the fixed
# baseline tau = 0.05 (value, not row position): PPML first when that count is
# positive, log-OLS first otherwise. Exactly one 0.05 baseline is required, so a
# future retune cannot silently reshuffle the panels. Ordering, not selection --
# flipping order never recomputes a number.
logvar_panel_order <- function(n_cross, tau, tau_baseline) {
  hit <- which(tau == tau_baseline)
  if (length(hit) != 1L) {
    stop("logvar_panel_order: exactly one baseline tau is required")
  }
  if (n_cross[[hit]] > 0) c("ppml", "logols") else c("logols", "ppml")
}
