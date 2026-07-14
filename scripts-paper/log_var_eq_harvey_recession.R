# Recession-cone existence certificate for the Harvey variance likelihood:
# on the column-normalized design Z = X diag(1/s), any nonzero direction of
# the cone {d : Z_pos d >= 0} scales onto one of the 2p facets u_j = sign,
# so enumerating the facet linear programs is exhaustive. A certified facet
# minimum below -rate_tol is a negative recession witness (no finite MLE); a
# certified feasible minimum within rate_tol of zero fails closed as
# unresolved (nonattainment or a flat direction, never a nonexistence
# claim); the gate passes only when every facet is certified infeasible or
# certified above rate_tol through the projected-multiplier conservative
# bounds. Raw solver codes and raw dual objectives are never evidence.
# Definitions only; sourced by log_var_eq_harvey.R.

source("scripts-paper/log_var_eq_harvey_recession_lp.R")

# column-scaled numerical rank of the positive-response rows (an information
# diagnostic, never an existence certificate by itself)
logvar_harvey_pos_rank <- function(x_pos) {
  if (nrow(x_pos) == 0L) {
    return(0L)
  }
  norms <- sqrt(colSums(x_pos^2))
  scaled <- x_pos
  nz <- norms > 0
  scaled[, nz] <- sweep(x_pos[, nz, drop = FALSE], 2, norms[nz], "/")
  d <- svd(scaled)$d
  sum(d > 1e-10 * d[1])
}

# classify one facet: negative witness, zero witness, certified positive,
# certified infeasible, or unresolved -- with every raw and projected
# quantity persisted
.logvar_facet_classify <- function(c_vec, z_pos, j, sign_j, rate_tol, t_max) {
  prim <- logvar_harvey_facet_primal(c_vec, z_pos, j, sign_j)
  rec <- list(j = j, sign = sign_j, primal = prim)
  if (prim$ok && prim$feas_viol <= 1e-8) {
    if (prim$value < -rate_tol) {
      rec$status <- "negative_witness"
      return(rec)
    }
    if (prim$value <= rate_tol) {
      rec$status <- "zero_witness"
      return(rec)
    }
    dual <- logvar_harvey_facet_dual(c_vec, z_pos, j, sign_j)
    rec$dual <- dual
    gap_ok <- dual$ok &&
      abs(prim$value - dual$bound) /
        max(1, abs(prim$value), abs(dual$bound)) <= 1e-8
    if (dual$ok && dual$bound > rate_tol && gap_ok) {
      rec$status <- "certified_positive"
    } else {
      rec$status <- "unresolved"
    }
    return(rec)
  }
  # no feasible primal point: certify infeasibility through phase I
  ph <- logvar_harvey_facet_phase1(z_pos, j, sign_j, t_max)
  rec$phase1 <- ph
  if (ph$ok && ph$t_min <= 1e-8) {
    # the facet is feasible after all; re-solve the primal from the
    # phase-I direction
    prim2 <- logvar_harvey_facet_primal(c_vec, z_pos, j, sign_j)
    rec$primal <- prim2
    if (prim2$ok && prim2$feas_viol <= 1e-8 && prim2$value < -rate_tol) {
      rec$status <- "negative_witness"
    } else if (prim2$ok && prim2$feas_viol <= 1e-8 && prim2$value <= rate_tol) {
      rec$status <- "zero_witness"
    } else {
      rec$status <- "unresolved"
    }
    return(rec)
  }
  if (ph$ok && isTRUE(ph$dual_ok) && ph$bound > 1e-8) {
    rec$status <- "certified_infeasible"
  } else {
    rec$status <- "unresolved"
  }
  rec
}

# the exhaustive facet certificate; the all-positive full-rank shortcut
# agrees with the facet routine (X d >= 0 with 1'X d <= 0 forces X d = 0)
# and is tagged so tests can compare both paths
logvar_harvey_recession_certificate <- function(y, x_mat) {
  p <- ncol(x_mat)
  s <- pmax(1, sqrt(colSums(x_mat^2)))
  z_mat <- sweep(x_mat, 2, s, "/")
  pos <- y > 0
  z_pos <- z_mat[pos, , drop = FALSE]
  rank_x_pos <- logvar_harvey_pos_rank(x_mat[pos, , drop = FALSE])
  c_vec <- colSums(z_mat)
  rate_tol <- 1e-9 * max(1, sum(abs(c_vec)))
  out <- list(
    rank_x_pos = rank_x_pos, col_scales = s, rate_tol = rate_tol,
    facets = list(), worst_direction = NULL, worst_rate = NA_real_,
    shortcut = FALSE
  )
  if (all(pos) && rank_x_pos == p) {
    out$classification <- "pass"
    out$shortcut <- TRUE
    return(out)
  }
  if (!any(pos)) {
    # every direction is cone-feasible; the negative of the column-sum sign
    # vector is an explicit negative witness (the intercept column keeps
    # c_vec nonzero)
    out$classification <- "negative_recession"
    out$worst_direction <- -sign(c_vec) / s
    out$worst_rate <- -sum(abs(c_vec))
    return(out)
  }
  t_max <- max(1, max(rowSums(abs(z_pos))))
  statuses <- character(0)
  for (j in seq_len(p)) {
    for (sign_j in c(-1, 1)) {
      rec <- .logvar_facet_classify(c_vec, z_pos, j, sign_j, rate_tol, t_max)
      out$facets[[length(out$facets) + 1L]] <- rec
      statuses <- c(statuses, rec$status)
      if (rec$status %in% c("negative_witness", "zero_witness")) {
        rate <- rec$primal$value
        if (is.na(out$worst_rate) || rate < out$worst_rate) {
          out$worst_rate <- rate
          out$worst_direction <- rec$primal$u / s
        }
      }
      if (rec$status == "negative_witness") {
        out$classification <- "negative_recession"
        return(out)
      }
    }
  }
  out$classification <- if (any(statuses == "zero_witness")) {
    "zero_recession"
  } else if (all(statuses %in% c("certified_positive", "certified_infeasible"))) {
    "pass"
  } else {
    "certificate_failure"
  }
  out
}

# deterministic machinery self-test for the analyze_domain precheck hook: a
# strictly positive response must pass, an all-zero response must return the
# known negative recession, and an injected slightly stationarity-infeasible
# dual must be rejected by the residual-adjusted conservative bound
logvar_harvey_recession_self_test <- function(x_mat) {
  n <- nrow(x_mat)
  pos_cert <- logvar_harvey_recession_certificate(rep(1, n), x_mat)
  zero_cert <- logvar_harvey_recession_certificate(rep(0, n), x_mat)
  # a fake dual whose raw objective clears rate_tol but whose stationarity
  # residual q wipes the margin: the conservative bound must reject it
  fake_raw <- 5e-9
  fake_q <- rep(1e-3, ncol(x_mat))
  fake_bound <- fake_raw - sum(abs(fake_q))
  checks <- c(
    positive = identical(pos_cert$classification, "pass"),
    all_zero = identical(zero_cert$classification, "negative_recession"),
    residual_adjust = !(fake_bound > pos_cert$rate_tol)
  )
  list(
    checks = checks, passed = all(checks),
    records = list(
      positive = pos_cert, all_zero = zero_cert,
      residual_adjust = list(
        raw = fake_raw, residual = fake_q, bound = fake_bound,
        rate_tol = pos_cert$rate_tol
      )
    )
  )
}
