# Fixed-schema row assembler, the conditional L-infinity epigraph trigger, and the
# tau == 0 singleton row for the joint-null diagnostic. The template pins every
# column's type (identifiers/enums character, counts/ranks integer, flags logical,
# coefficients/distances/tolerances double) so the CSV and RDS artifacts round-
# trip under a declared manifest; sample_id, tau_order, and nearest_crossing_qtr
# stay typed NA for the driver to fill. The L-infinity refinement is a predeclared
# sensitivity: it fires only on the pinned absolute-plus-relative gap and defers
# the solve to the search module's epigraph program. Definitions only; sourced by
# the at-tau orchestration.

# fixed-schema template with the manifest's column types as typed NA defaults
.jn_row_template <- function() {
  list(
    schema_version = "1.0.0", estimator = "logols",
    diagnostic = "joint_null_theta_r", sample_id = NA_character_,
    inference_status = "deferred", tau_order = NA_integer_,
    tau_display = NA_real_, tau_internal = NA_real_,
    scaled_l2 = NA_real_, scaled_linf = NA_real_,
    b1 = NA_real_, b2 = NA_real_, b3 = NA_real_,
    theta0 = NA_real_, thetaR1 = NA_real_, thetaR2 = NA_real_,
    thetaR3 = NA_real_, thetaR4 = NA_real_,
    min_abs_eps = NA_real_, nearest_crossing_qtr = NA_character_,
    constraint_residual = NA_real_, raw_gradient_norm = NA_real_,
    grid_points = NA_integer_, n_unavailable_grid = NA_integer_,
    polish_starts = NA_integer_, successful_polishes = NA_integer_,
    argmin_source = NA_character_, winning_start_type = NA_character_,
    n_agreeing_starts = NA_integer_, start_agreement_max_scaled = NA_real_,
    basin_count = NA_integer_, replication_status = NA_character_,
    perturbation_status = NA_character_, stability_status = NA_character_,
    n_near_crossing_rows = NA_integer_, status = NA_character_,
    membership_result = NA_character_, root_tol = NA_real_,
    scale1 = NA_real_, scale2 = NA_real_, scale3 = NA_real_, scale4 = NA_real_,
    box_active = NA, sparse_grid = NA,
    metric_l2_source = NA_character_, metric_linf_source = NA_character_,
    l2_rank_under_linf = NA_integer_, linf_rank_under_l2 = NA_integer_,
    d_inf_l2 = NA_real_, d_inf_grid_best = NA_real_,
    linf_gap_abs = NA_real_, linf_gap_rel = NA_real_, linf_trigger = NA,
    linf_sensitivity_status = NA_character_
  )
}

# fill the template, failing closed on any unexpected column name and stripping
# stray element names so the flat row round-trips byte-for-byte
.jn_assemble_row <- function(vals) {
  row <- .jn_row_template()
  unknown <- setdiff(names(vals), names(row))
  if (length(unknown)) {
    stop(sprintf("joint-null row: unknown field(s) %s", paste(unknown, collapse = ", ")))
  }
  for (nm in names(vals)) row[[nm]] <- unname(vals[[nm]])
  row
}

# map the scan's winning-start tag onto the closed winning_start_type enum
.jn_win_type <- function(t) {
  ok <- c("grid_arg", "grid_pool", "b_seed", "prior_tau", "carry")
  if (length(t) == 1L && t %in% ok) t else "grid_arg"
}

# the not-triggered L-infinity record used when the primary tau is already
# unreliable (the near-puncture candidates carry no trustworthy sensitivity)
.jn_linf_untriggered <- function(dinf_l2, l2_src) {
  list(
    trigger = FALSE, status = "not_triggered", gap_abs = NA_real_,
    gap_rel = NA_real_, dinf_l2 = dinf_l2, dinf_grid = NA_real_, l2_src = l2_src,
    linf_src = NA_character_, l2_rank = NA_integer_, linf_rank = NA_integer_
  )
}

# provenance tag of a separated start from its start_type attribute (grid or
# carry in this diagnostic; defaults to grid when the attribute is unset)
.jn_start_source <- function(s0) {
  t <- attr(s0, "start_type")
  if (is.null(t) || !length(t)) "grid" else as.character(t)[1L]
}

# Conditional sup-norm sensitivity: over the attained candidate pool retain the
# L2 winner (b_hat) and the smallest-d_inf candidate, with each candidate's rank
# under the other metric. Trigger only when the two differ and the pinned
# absolute-plus-relative gap is exceeded, then defer the refinement to the search
# module's epigraph program (a failed solve degrades to attained_start_only). Each
# candidate carries its provenance so the two metric-winner sources are recorded
# from the actual winner, not a hardcoded guess.
.jn_linf_sensitivity <- function(b_hat, scaled_linf, argmin_source, scan, polish,
                                 qs, w1, w2, proj, d_inv2, root_tol) {
  cand <- list(b_hat)
  src <- argmin_source
  add <- function(x, s) {
    if (!is.null(x) && length(x) == length(b_hat) && all(is.finite(x))) {
      cand[[length(cand) + 1L]] <<- as.numeric(x)
      src[[length(src) + 1L]] <<- s
    }
  }
  add(scan$best$b, scan$best$source)
  for (s0 in scan$starts) add(as.numeric(s0), .jn_start_source(s0))
  add(polish$best$b, "polish")
  for (rec in polish$records) if (!is.null(rec)) add(rec$par, "polish")
  qv <- dv <- numeric(length(cand))
  for (i in seq_along(cand)) {
    o <- logvar_joint_null_objective(cand[[i]], w1, w2, proj, d_inv2)
    fin <- is.finite(o$q)
    qv[i] <- if (fin) as.numeric(o$q) else Inf
    dv[i] <- if (fin) max(abs(as.numeric(o$s) * sqrt(d_inv2))) else Inf
  }
  best_i <- which.min(dv)
  dinf_grid <- dv[best_i]
  differ <- sqrt(sum((cand[[best_i]] - b_hat)^2)) > 1e-12
  gap_abs <- dv[1L] - dinf_grid
  gap_rel <- gap_abs / max(root_tol, dinf_grid)
  trigger <- isTRUE(differ && is.finite(gap_abs) && gap_abs > 10 * root_tol &&
    gap_rel > 0.10)
  status <- "not_triggered"
  if (trigger) {
    epi <- tryCatch(
      logvar_joint_null_epigraph(
        qs, list(b_hat, cand[[best_i]]), w1, w2, proj,
        d_inv2, root_tol
      ),
      error = function(e) NULL
    )
    status <- if (is.null(epi)) "attained_start_only" else epi$status
  }
  list(
    trigger = trigger, status = status, gap_abs = gap_abs, gap_rel = gap_rel,
    dinf_l2 = dv[1L], dinf_grid = dinf_grid,
    l2_src = src[[1L]], linf_src = src[[best_i]],
    l2_rank = as.integer(rank(dv, ties.method = "min")[1L]),
    linf_rank = as.integer(rank(qv, ties.method = "min")[best_i])
  )
}

# The tau == 0 Lewbel singleton: a single-point evaluation of the display box
# centre with an explicit fail-closed rule (finite off-crossing map, feasibility,
# root-tolerance sup-norm) and not_applicable start/perturbation fields.
.jn_singleton_row <- function(tau_display, b_tab, w1, w2, proj, d_inv2, qs, omega,
                              scales, e_scale_ref, root_tol) {
  lower <- as.numeric(b_tab$set_lower)
  upper <- as.numeric(b_tab$set_upper)
  b <- (lower + upper) / 2
  o <- logvar_joint_null_objective(b, w1, w2, proj, d_inv2)
  e <- drop(w1 - w2 %*% b)
  min_abs_e <- min(abs(e))
  slopes <- as.numeric(o$s)
  theta <- as.numeric(o$theta)
  q <- as.numeric(o$q)
  scaled_linf <- max(abs(slopes * sqrt(d_inv2)))
  feas <- .feasibility_residual(qs, b, omega)
  grad <- tryCatch(logvar_joint_null_gradient(b, w1, w2, proj, d_inv2),
    error = function(e) NA_real_
  )
  crossing <- any(e == 0) || min_abs_e <= 1e-6 * e_scale_ref || !is.finite(q)
  feasible <- is.finite(feas) && feas <= 1e-4
  if (crossing || !feasible) {
    status <- "unreliable"
    membership <- "compatibility_not_demonstrated"
  } else if (scaled_linf <= root_tol) {
    status <- "bounded"
    membership <- "compatible_witness"
  } else {
    status <- "bounded"
    membership <- "compatibility_not_demonstrated"
  }
  span <- pmax(abs(upper - lower), 1)
  # a degenerate singleton box has span 0, so |b - bound| = 0 would spuriously read
  # as an edge-riding arg-min; the tau = 0 point is never box-limited
  box_active <- !all(upper == lower) &&
    (any(abs(b - lower) <= 1e-6 * span) || any(abs(b - upper) <= 1e-6 * span))
  .jn_assemble_row(list(
    tau_display = as.numeric(tau_display), tau_internal = 0,
    scaled_l2 = sqrt(2 * q), scaled_linf = scaled_linf,
    b1 = b[1], b2 = b[2], b3 = b[3], theta0 = theta[1],
    thetaR1 = theta[2], thetaR2 = theta[3], thetaR3 = theta[4], thetaR4 = theta[5],
    min_abs_eps = min_abs_e, constraint_residual = feas,
    raw_gradient_norm = if (all(is.finite(grad))) sqrt(sum(grad^2)) else NA_real_,
    argmin_source = "grid", winning_start_type = "b_seed",
    replication_status = "not_applicable_singleton",
    perturbation_status = "not_applicable_singleton",
    stability_status = "not_applicable_singleton",
    n_near_crossing_rows = as.integer(sum(abs(e) <= 1e-6 * e_scale_ref)),
    status = status, membership_result = membership, root_tol = root_tol,
    scale1 = scales[1], scale2 = scales[2], scale3 = scales[3], scale4 = scales[4],
    box_active = box_active, sparse_grid = FALSE, metric_l2_source = "grid",
    d_inf_l2 = scaled_linf, linf_trigger = FALSE,
    linf_sensitivity_status = "not_triggered"
  ))
}
