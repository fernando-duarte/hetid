# Fixed-schema row assembly and L-infinity sensitivity for joint-null.

paper_source_once(paper_path(
  "log_variance", "diagnostics", "joint_null", "schema.R"
))

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
                                 qs, w1, w2, proj, d_inv2, root_tol, control) {
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
  differ <- sqrt(sum((cand[[best_i]] - b_hat)^2)) >
    control$perturbation_dedupe_tol
  gap_abs <- dv[1L] - dinf_grid
  gap_rel <- gap_abs / max(root_tol, dinf_grid)
  trigger <- isTRUE(
    differ &&
      is.finite(gap_abs) &&
      gap_abs > control$linf_gap_root_multiplier * root_tol &&
      gap_rel > control$linf_gap_rel_tol
  )
  status <- "not_triggered"
  if (trigger) {
    epi <- tryCatch(
      logvar_joint_null_epigraph(
        qs, list(b_hat, cand[[best_i]]), w1, w2, proj,
        d_inv2, control
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
                              scales, e_scale_ref, root_tol, control) {
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
  crossing <- any(e == 0) ||
    min_abs_e <= control$crossing_rel_tol * e_scale_ref ||
    !is.finite(q)
  feasible <- is.finite(feas) && feas <= control$feasibility_tol
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
    (
      any(abs(b - lower) <= control$box_active_rel_tol * span) ||
        any(abs(b - upper) <= control$box_active_rel_tol * span)
    )
  .jn_assemble_row(c(
    list(
      tau_display = as.numeric(tau_display), tau_internal = 0,
      scaled_l2 = sqrt(2 * q), scaled_linf = scaled_linf
    ),
    .jn_parameter_fields(b, theta, scales),
    list(
      min_abs_eps = min_abs_e, constraint_residual = feas,
      raw_gradient_norm = if (all(is.finite(grad))) {
        sqrt(sum(grad^2))
      } else {
        NA_real_
      },
      argmin_source = "grid", winning_start_type = "b_seed",
      replication_status = "not_applicable_singleton",
      perturbation_status = "not_applicable_singleton",
      stability_status = "not_applicable_singleton",
      n_near_crossing_rows = as.integer(sum(
        abs(e) <= control$crossing_rel_tol * e_scale_ref
      )),
      status = status, membership_result = membership, root_tol = root_tol,
      box_active = box_active, sparse_grid = FALSE,
      metric_l2_source = "grid", d_inf_l2 = scaled_linf,
      linf_trigger = FALSE, linf_sensitivity_status = "not_triggered"
    )
  ))
}
