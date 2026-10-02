# Endpoint diagnostics for the regularized log projections (spec "Endpoint
# searches and retained results"): one flat row per method, multiplier, tau,
# coefficient, and side, joining the reconciled engine schema and the audit
# frame with the package diagnostics re-evaluated at the attaining candidate,
# plus reference and Lewbel-point rows. Evaluation status is kept apart from the
# endpoint status, and a missing attaining candidate keeps its row with NA
# evaluation fields. Definitions only; sourced by run_sets.R.

# package diagnostics at one candidate, flattened to natural-scale scalars
logvar_log_projection_eval_fields <- function(prep, method, multiplier, b,
                                              coef_labels) {
  theta0 <- stats::setNames(rep(NA_real_, length(coef_labels)), coef_labels)
  out <- list(
    evaluation_status = NA_character_, h_T = NA_real_, s_b = NA_real_,
    c_T = NA_real_, share_small = NA_real_, profile_log_ratio_min = NA_real_,
    profile_log_ratio_max = NA_real_
  )
  if (!is.null(b) && all(is.finite(b))) {
    ev <- hetid::evaluate_log_projection(prep, unname(b), method, multiplier,
      jacobian = FALSE
    )
    d <- ev$diagnostics
    out$evaluation_status <- ev$status
    if (!is.null(d$share_small)) out$share_small <- d$share_small
    if (!is.null(d$log_threshold)) out$h_T <- exp(d$log_threshold / 2)
    if (!is.null(d$log_scale)) out$s_b <- exp(d$log_scale / 2)
    if (!is.null(d$log_c)) out$c_T <- exp(d$log_c)
    if (!is.null(d$profile_log_ratio_min)) {
      out$profile_log_ratio_min <- d$profile_log_ratio_min
      out$profile_log_ratio_max <- d$profile_log_ratio_max
    }
    if (!is.null(d$first_pass_coef)) theta0[] <- d$first_pass_coef
  }
  c(out, stats::setNames(as.list(theta0), paste0("theta0_", coef_labels)))
}

logvar_log_projection_endpoint_rows <- function(mapped, prep, ctx) {
  est <- mapped$estimator
  method <- est$metadata$fit_control$method
  labels <- est$coef_labels
  news <- colnames(prep$w2)
  row_of <- function(tau, coef, side, role, value, status, reason, provenance,
                     residual, delta, tol, b) {
    b_cols <- stats::setNames(
      as.list(if (is.null(b)) rep(NA_real_, length(news)) else unname(b)),
      paste0("b_", news)
    )
    data.frame(c(
      list(
        id = est$metadata$estimator, multiplier = mapped$multiplier, tau = tau,
        coef = coef, side = side, role = role, endpoint_value = value,
        endpoint_status = status, reason = reason, provenance = provenance,
        constraint_residual = residual, audit_delta = delta, audit_tol = tol
      ),
      b_cols,
      list(
        n_mean = attr(prep, "n_mean"), n_vol = attr(prep, "n_vol"),
        s_hat = exp(prep$log_scale_common / 2)
      ),
      logvar_log_projection_eval_fields(prep, method, mapped$multiplier, b, labels),
      list(sample_id = est$metadata$sample_id, spec_id = est$metadata$spec_id)
    ), stringsAsFactors = FALSE, check.names = FALSE)
  }
  audit <- mapped$audit
  rows <- list()
  for (res in mapped$final) {
    sch <- res$schema
    for (j in seq_len(nrow(sch))) {
      for (side in c("lower", "upper")) {
        hit <- if (is.null(audit)) {
          integer(0)
        } else {
          which(abs(audit$tau - sch$tau[[j]]) < 1e-12 &
            audit$coef == sch$coef[[j]] & audit$side == side)
        }
        arg <- sch[[paste0("arg_", side)]][[j]]
        rows[[length(rows) + 1L]] <- row_of(
          sch$tau[[j]], sch$coef[[j]], side, "endpoint",
          sch[[side]][[j]], sch[[paste0(side, "_status")]][[j]],
          if (length(hit)) audit$reason[[hit[1L]]] else NA_character_,
          sch[[paste0(side, "_provenance")]][[j]],
          sch[[paste0(side, "_constraint_residual")]][[j]],
          if (length(hit)) audit$delta[[hit[1L]]] else NA_real_,
          if (length(hit)) audit$tol[[hit[1L]]] else NA_real_,
          if (all(is.finite(arg))) arg else NULL
        )
      }
    }
  }
  for (role in c("reference", "point")) {
    b <- if (role == "reference") ctx$b_ref else if (isTRUE(ctx$point_feasible)) ctx$b_point
    rows[[length(rows) + 1L]] <- row_of(
      NA_real_, NA_character_, "na", role, NA_real_, NA_character_,
      if (is.null(b)) "point_infeasible" else NA_character_,
      NA_character_, NA_real_, NA_real_, NA_real_, b
    )
  }
  do.call(rbind, rows)
}
