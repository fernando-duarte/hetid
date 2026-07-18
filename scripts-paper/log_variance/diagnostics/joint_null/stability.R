# Crossing-geometry stability protocol and the six-condition witness geometry for
# the joint-null theta_R = 0 diagnostic. At an apparent witness b_hat the slope
# rows of the log-variance map can diverge (leverage) or cancel near a puncture,
# so a small attained slope norm is trusted only when it survives feasibility-
# checked perturbations that provably leave any cancellation manifold: the six
# signed coordinate axes plus the signed unit normals +/- w2_i / ||w2_i|| of every
# active near-crossing row. Radii are 0.01/0.03/0.1 of the constraint theta-scale;
# matched directions (feasible at all radii) must span (rank_dir == 3), and a
# two-endpoint puncture signature or a machine-precision-adjacent residual fails
# the candidate closed. This works in raw slope units; the sd-effect scaling is
# owned by the at-tau orchestrator (which holds d_inv2). This entry file sources
# the at-tau orchestration and row assembler so run_pipeline.R and the joint-null
# tests see one stability surface. Definitions only.

paper_source_once(paper_path("log_variance", "diagnostics", "joint_null", "at_tau.R"))

# unit perturbation directions: the 2K signed coordinate axes, then the signed
# unit normals of each active row; owner records which active row a column serves
# (0 for an axis) so coverage can require a matched normal per active row
.jn_perturb_directions <- function(k, w2, active, norms) {
  dirs <- cbind(diag(k), -diag(k))
  owner <- rep(0L, ncol(dirs))
  for (i in active) {
    u <- w2[i, ] / norms[i]
    dirs <- cbind(dirs, u, -u)
    owner <- c(owner, i, i)
  }
  list(dirs = dirs, owner = owner)
}

# feasibility, Euclidean slope norm d_2, sup slope norm d_inf, and min|e| at every
# (direction, radius) through the shared zero-safe response, so a tiny nonzero
# residual stays a huge-but-finite slope rather than a faked puncture; an
# infeasible, exact-zero, or non-finite-map point stays NA and never enters the
# matched-direction set
.jn_perturb_eval <- function(b, w1, w2, proj, qs, omega, dirs, radii, feas_tol) {
  nd <- ncol(dirs)
  nr <- length(radii)
  feasible <- matrix(FALSE, nd, nr)
  d2 <- dinf <- mae <- matrix(NA_real_, nd, nr)
  for (j in seq_len(nd)) {
    for (ri in seq_len(nr)) {
      cand <- b + radii[ri] * dirs[, j]
      if (.feasibility_residual(qs, cand, omega) <= feas_tol) {
        feasible[j, ri] <- TRUE
        e <- drop(w1 - w2 %*% cand)
        resp <- logvar_joint_null_response(e)
        if (length(resp$available_ids)) {
          s <- drop(proj %*% resp$log_sq)[-1]
          if (all(is.finite(s))) {
            d2[j, ri] <- sqrt(sum(s^2))
            dinf[j, ri] <- max(abs(s))
          }
        }
        mae[j, ri] <- min(abs(e))
      }
    }
  }
  list(feasible = feasible, d2 = d2, dinf = dinf, mae = mae)
}

# rank of the matched unsigned direction set: canonicalize each column's sign so
# its first non-negligible component is positive, deduplicate rows at 1e-12, and
# count SVD singular values above the 1e-10 relative threshold (0 for no rows)
.jn_matched_rank <- function(dmat, control) {
  if (!ncol(dmat)) {
    return(0L)
  }
  cano <- vapply(seq_len(ncol(dmat)), function(cc) {
    v <- dmat[, cc]
    nz <- which(abs(v) > control$perturbation_sign_tol)
    if (length(nz) && v[nz[1L]] < 0) -v else v
  }, numeric(nrow(dmat)))
  rows <- t(cano)
  keep <- rep(TRUE, nrow(rows))
  for (ia in seq_len(nrow(rows) - 1L)) {
    if (!keep[ia]) next
    for (ib in (ia + 1L):nrow(rows)) {
      distance <- sqrt(sum((rows[ia, ] - rows[ib, ])^2))
      if (keep[ib] && distance < control$perturbation_dedupe_tol) {
        keep[ib] <- FALSE
      }
    }
  }
  sv <- svd(rows[keep, , drop = FALSE])$d
  if (!length(sv)) {
    0L
  } else {
    sum(sv > control$perturbation_rank_tol * max(sv))
  }
}

# two-endpoint puncture signature: as the radius shrinks 0.1 -> 0.01 a genuine
# cancellation collapses BOTH the slope norm and min|e| below 0.15 of their
# largest-radius values; any matched direction that does so fails the witness
.jn_puncture_signature <- function(ev, matched, control) {
  for (j in matched) {
    last_radius <- ncol(ev$d2)
    if (isTRUE(
      ev$d2[j, 1L] <
        control$puncture_ratio * ev$d2[j, last_radius]
    ) &&
      isTRUE(
        ev$mae[j, 1L] <
          control$puncture_ratio * ev$mae[j, last_radius]
      )) {
      return(TRUE)
    }
  }
  FALSE
}

# Stability verdict at an apparent witness b_hat: min|e| leverage census, the
# perturbation coverage/rank/puncture protocol, and the scoped off-manifold
# persistence check. Returns status/stability_status/perturbation_status/rank_dir/
# membership_result plus the active-row detail the at-tau row and RDS record.
logvar_joint_null_stability <- function(
  b_hat, w1, w2, proj, qs, eps_ref, root_tol,
  control = LOGVAR_JOINT_NULL_CONTROL
) {
  k <- length(b_hat)
  e_scale_ref <- stats::median(abs(eps_ref))
  stopifnot(is.finite(e_scale_ref), e_scale_ref > 0)
  e_hat <- drop(w1 - w2 %*% b_hat)
  min_abs_e <- min(abs(e_hat))
  active <- which(
    abs(e_hat) <= control$crossing_rel_tol * e_scale_ref
  )
  norms <- sqrt(rowSums(w2^2))
  verdict <- function(status, stab, pert, rank, mem, reason) {
    list(
      status = status, stability_status = stab, perturbation_status = pert,
      rank_dir = rank, membership_result = mem, min_abs_eps = min_abs_e,
      n_active_rows = length(active), active_rows = active, reason = reason
    )
  }
  # machine-precision adjacency: closer to a crossing than 1e-8 of the e-scale is
  # a punctured-domain limit, not an attained root
  if (min_abs_e <= control$machine_adjacent_rel_tol * e_scale_ref) {
    return(verdict(
      "unreliable", "fail", "fail", NA_integer_,
      "compatibility_not_demonstrated", "machine_precision_adjacent"
    ))
  }
  # an active row with a zero-norm w2 has no defined perturbation normal
  if (length(active) && any(norms[active] == 0)) {
    return(verdict(
      "unreliable", "fail", "fail", NA_integer_,
      "compatibility_not_demonstrated", "active_zero_norm_row"
    ))
  }
  theta_scale <- .derive_theta_scale(qs)
  omega <- .derive_constraint_scales(qs, theta_scale)
  radii <- control$perturbation_fractions * theta_scale
  built <- .jn_perturb_directions(k, w2, active, norms)
  ev <- .jn_perturb_eval(
    b_hat, w1, w2, proj, qs, omega, built$dirs, radii,
    control$feasibility_tol
  )
  usable <- ev$feasible & is.finite(ev$d2) & is.finite(ev$mae)
  matched <- which(apply(usable, 1L, all))
  rank_dir <- .jn_matched_rank(
    built$dirs[, matched, drop = FALSE], control
  )
  covered <- !length(active) ||
    all(vapply(active, function(i) any(built$owner[matched] == i), logical(1)))
  if (rank_dir != k || !covered) {
    return(verdict(
      "unreliable", "fail", "insufficient_coverage", rank_dir,
      "compatibility_not_demonstrated", "insufficient_matched_perturbation_coverage"
    ))
  }
  if (.jn_puncture_signature(ev, matched, control)) {
    return(verdict(
      "unreliable", "fail", "fail", rank_dir,
      "compatibility_not_demonstrated", "puncture_signature"
    ))
  }
  # off-manifold near-zero persistence, required only where active rows exist: a
  # genuine isolated root stays near zero one small radius off every manifold
  if (length(active)) {
    sr <- which(ev$feasible[, 1L])
    if (!length(sr) || !all(is.finite(ev$dinf[sr, 1L])) ||
      any(
        ev$dinf[sr, 1L] >
          control$off_manifold_root_multiplier * root_tol
      )) {
      return(verdict(
        "unreliable", "fail", "fail", rank_dir,
        "compatibility_not_demonstrated", "off_manifold_distance"
      ))
    }
  }
  verdict("bounded", "pass", "pass", rank_dir, "compatible_witness", "stable")
}
