# Per-display-tau orchestration of the joint-null distance diagnostic: scan the
# joint feasible set, polish, classify a constructive witness (dossier 6.1), run
# the crossing stability protocol at any apparent witness, evaluate the
# conditional sup-norm epigraph sensitivity, and assemble one fixed-schema result
# row. Side-effect-free (no printing or writing) so the deferred inference round
# can map it over bootstrap resamples. tau == 0 is the single-point Lewbel
# singleton whose start/perturbation fields are not_applicable. Consumes the
# search module (scan/polish/epigraph), the math module (objective/gradient), and
# the sibling stability protocol; the row assembler and L-infinity trigger are in
# the companion file this sources. Definitions only.

source("scripts-paper/log_var_eq_joint_null_row.R")

# joint lattice resolution: the caller supplies none, so default to the benchmark
# grid_n, overridable by the driver through this option
.jn_grid_n <- function() as.integer(getOption("logvar_joint_null_grid_n", 13L))

# count independent polished starts that reproduce b_hat: per-coordinate
# coefficient agreement (tolerance scaled by the theta-scale, robust near the
# origin where witnesses live) and freshly recomputed objective agreement. A
# start reaching a distinct valid feasible minimum is basin diversity, not a
# replication failure.
.jn_replication <- function(b_hat, q_hat, records, w1, w2, proj, d_inv2, qs, root_tol) {
  scale <- .derive_theta_scale(qs)
  omega <- .derive_constraint_scales(qs, scale)
  tol_b <- 1e-6 * pmax(scale, abs(b_hat))
  agree <- 0L
  basins <- 0L
  max_scaled <- 0
  for (rec in records) {
    if (is.null(rec) || is.null(rec$par) || anyNA(rec$par)) next
    bb <- as.numeric(rec$par)
    if (length(bb) != length(b_hat)) next
    fr <- .feasibility_residual(qs, bb, omega)
    if (!is.finite(fr) || fr > 1e-4) next
    o <- logvar_joint_null_objective(bb, w1, w2, proj, d_inv2)
    if (!is.finite(o$q)) next
    q_tol <- max(64 * .Machine$double.eps * max(1, abs(o$q), abs(q_hat)), 4 * root_tol^2)
    coord_gap <- abs(bb - b_hat)
    if (all(coord_gap <= tol_b) && abs(as.numeric(o$q) - q_hat) <= q_tol) {
      agree <- agree + 1L
      max_scaled <- max(max_scaled, max(coord_gap / pmax(scale, abs(b_hat))))
    } else {
      basins <- basins + 1L
    }
  }
  list(
    agree = agree >= 2L, n_agreeing = agree, basin_count = 1L + basins,
    max_scaled = max_scaled
  )
}

# the crossing-honest six-condition witness gate (dossier 6.1): a finite off-
# crossing map, root-tolerance sup-norm, Lewbel feasibility, a bit-stable fresh
# recomputation, the stability protocol, and one reproducing independent start.
.jn_is_witness <- function(stab, rep, scaled_linf, feas, mach_adj, e_hat, fresh_ok,
                           root_tol) {
  identical(stab$status, "bounded") && !mach_adj && scaled_linf <= root_tol &&
    is.finite(feas) && feas <= 1e-4 && fresh_ok && rep$agree &&
    all(is.finite(e_hat)) && all(e_hat != 0)
}

# One per-display-tau result row. See the frozen signature: the driver adds
# tau_order and sample_id when it assembles the artifact, so those stay typed NA.
logvar_joint_null_at_tau <- function(tau, tau_display, b_tab, w1, w2, proj, d_inv2,
                                     eps_ref, qs, prior_minima = list(),
                                     root_tol = 1e-6) {
  scales <- 1 / sqrt(d_inv2)
  omega <- .derive_constraint_scales(qs, .derive_theta_scale(qs))
  e_scale_ref <- stats::median(abs(eps_ref))
  stopifnot(is.finite(e_scale_ref), e_scale_ref > 0)
  if (tau == 0) {
    return(.jn_singleton_row(
      tau_display, b_tab, w1, w2, proj, d_inv2, qs, omega,
      scales, e_scale_ref, root_tol
    ))
  }
  scan <- logvar_joint_null_scan(qs, b_tab, w1, w2, proj, d_inv2, .jn_grid_n(),
    prior_minima = prior_minima, seed_points = list()
  )
  polish <- logvar_joint_null_polish(qs, scan$starts, w1, w2, proj, d_inv2)
  b_hat <- scan$best$b
  argmin_source <- scan$best$source
  win_type <- scan$best$type
  if (is.finite(polish$best$q) && polish$best$q < scan$best$q) {
    b_hat <- polish$best$b
    argmin_source <- "polish"
  }
  o <- logvar_joint_null_objective(b_hat, w1, w2, proj, d_inv2)
  q_hat <- as.numeric(o$q)
  slopes <- as.numeric(o$s)
  theta <- as.numeric(o$theta)
  scaled_l2 <- sqrt(2 * q_hat)
  scaled_linf <- max(abs(slopes * sqrt(d_inv2)))
  e_hat <- drop(w1 - w2 %*% b_hat)
  min_abs_e <- min(abs(e_hat))
  feas <- .feasibility_residual(qs, b_hat, omega)
  grad <- tryCatch(logvar_joint_null_gradient(b_hat, w1, w2, proj, d_inv2),
    error = function(e) NA_real_
  )
  n_active <- sum(abs(e_hat) <= 1e-6 * e_scale_ref)
  apparent <- is.finite(scaled_linf) && scaled_linf <= root_tol
  mach_adj <- min_abs_e <= 1e-8 * e_scale_ref
  status <- "bounded"
  membership <- "compatibility_not_demonstrated"
  stab_status <- "not_required_nonwitness"
  pert_status <- "not_triggered_nonwitness"
  repl_status <- "not_required_nonwitness"
  rank_dir <- NA_integer_
  n_agree <- NA_integer_
  agree_max <- NA_real_
  basin_count <- NA_integer_
  if (isTRUE(scan$sparse_grid)) {
    status <- "unreliable"
  } else if (apparent || mach_adj) {
    stab <- logvar_joint_null_stability(b_hat, w1, w2, proj, qs, eps_ref, root_tol)
    rep <- .jn_replication(b_hat, q_hat, polish$records, w1, w2, proj, d_inv2, qs, root_tol)
    o2 <- logvar_joint_null_objective(b_hat, w1, w2, proj, d_inv2)
    fresh_ok <- identical(as.numeric(o2$q), q_hat) &&
      max(abs(as.numeric(o2$s) - slopes)) <= 1e-10 * max(1, max(abs(slopes)))
    stab_status <- stab$stability_status
    pert_status <- stab$perturbation_status
    rank_dir <- stab$rank_dir
    n_agree <- rep$n_agreeing
    agree_max <- rep$max_scaled
    basin_count <- rep$basin_count
    repl_status <- if (rep$agree) "agree" else "disagree"
    if (.jn_is_witness(stab, rep, scaled_linf, feas, mach_adj, e_hat, fresh_ok, root_tol)) {
      membership <- "compatible_witness"
    } else {
      status <- "unreliable"
    }
  }
  linf <- if (identical(status, "unreliable")) {
    .jn_linf_untriggered(scaled_linf, argmin_source)
  } else {
    .jn_linf_sensitivity(
      b_hat, scaled_linf, argmin_source, scan, polish, qs, w1, w2, proj,
      d_inv2, root_tol
    )
  }
  lower <- as.numeric(b_tab$set_lower)
  upper <- as.numeric(b_tab$set_upper)
  span <- pmax(abs(upper - lower), 1)
  box_active <- any(abs(b_hat - lower) <= 1e-6 * span) ||
    any(abs(b_hat - upper) <= 1e-6 * span)
  # every successful polished local minimum plus the arg-min, carried as an
  # attribute so the driver reseeds each prior basin without touching the
  # flat-row/$-field interface (the deferred inference round reads $-fields only)
  jn_minima <- Filter(
    function(bb) length(bb) == length(b_hat) && all(is.finite(bb)),
    c(list(as.numeric(b_hat)), lapply(polish$minima, as.numeric))
  )
  structure(.jn_assemble_row(list(
    tau_display = as.numeric(tau_display), tau_internal = as.numeric(tau),
    scaled_l2 = scaled_l2, scaled_linf = scaled_linf,
    b1 = b_hat[1], b2 = b_hat[2], b3 = b_hat[3], theta0 = theta[1],
    thetaR1 = theta[2], thetaR2 = theta[3], thetaR3 = theta[4], thetaR4 = theta[5],
    min_abs_eps = min_abs_e, constraint_residual = feas,
    raw_gradient_norm = if (all(is.finite(grad))) sqrt(sum(grad^2)) else NA_real_,
    grid_points = as.integer(scan$grid_points),
    n_unavailable_grid = as.integer(scan$n_unavailable),
    polish_starts = length(scan$starts),
    successful_polishes = as.integer(polish$successful_polishes),
    argmin_source = argmin_source, winning_start_type = .jn_win_type(win_type),
    n_agreeing_starts = n_agree, start_agreement_max_scaled = agree_max,
    basin_count = basin_count, replication_status = repl_status,
    perturbation_status = pert_status, stability_status = stab_status,
    n_near_crossing_rows = as.integer(n_active), status = status,
    membership_result = membership, root_tol = root_tol,
    scale1 = scales[1], scale2 = scales[2], scale3 = scales[3], scale4 = scales[4],
    box_active = box_active, sparse_grid = isTRUE(scan$sparse_grid),
    metric_l2_source = linf$l2_src, metric_linf_source = linf$linf_src,
    l2_rank_under_linf = linf$l2_rank, linf_rank_under_l2 = linf$linf_rank,
    d_inf_l2 = linf$dinf_l2, d_inf_grid_best = linf$dinf_grid,
    linf_gap_abs = linf$gap_abs, linf_gap_rel = linf$gap_rel,
    linf_trigger = linf$trigger, linf_sensitivity_status = linf$status
  )), minima = jn_minima)
}
