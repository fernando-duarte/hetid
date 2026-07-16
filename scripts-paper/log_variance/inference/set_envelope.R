# Centered one-sided max-root OUTER confidence envelope for the vol-equation
# set-identified endpoints, per the two bespoke bootstrap reports. Target: whole-
# set containment P{[L_j,U_j] subset [C_L,C_U]} >= 1-alpha, NOT point coverage
# (so NOT Imbens-Manski/Stoye) and NOT percentiles/BCa (both endpoints can move
# inward in one draw). Reuses only robust_scale (MAD, sd-consistent, location-
# invariant) and boot_min_reps from identified_set_inference.R. Half-infinite sets use
# the single live side; a divergent-draw fraction or a degenerate scale fails the
# regularity gate (divergent draws are counted, never dropped). Consumed by
# scripts-paper/log_variance/inference/run_set_bootstrap.R.

source(paper_path("support", "identification", "identified_set_inference.R"))

# Conservative (1-alpha) order statistic of the bootstrap max-root, rank
# ceil((B+1)(1-alpha)) capped at B (Politis-Romano-Wolf).
logvar_root_critical <- function(root, alpha) {
  root <- root[is.finite(root)]
  if (!length(root)) {
    return(NA_real_)
  }
  k <- min(length(root), ceiling((length(root) + 1) * (1 - alpha)))
  sort(root, partial = k)[k]
}

# Per-side statistics and the regularity gate. `inward_sign` is +1 for the lower
# side (D_L = vals - anchor) and -1 for the upper (D_U = anchor - vals). The scale
# is the MAD of the bounded draw endpoints (location-invariant, so anchor-free);
# the studentized inward deviation z is defined only on bounded draws. The gate
# needs a finite anchor, >= min_reps bounded draws, a bounded fraction >=
# stability (divergent draws counted, not dropped), and a positive finite scale.
logvar_side_stat <- function(vals, status, anchor, inward_sign, min_reps,
                             stability) {
  ok <- is.finite(vals) & status == "bounded"
  n_valid <- sum(status != "failed")
  frac <- if (n_valid > 0L) sum(ok) / n_valid else 0
  se <- if (sum(ok) >= 2L) robust_scale(vals[ok]) else NA_real_
  reason <- NA_character_
  gate <- is.finite(anchor)
  if (gate && sum(ok) < min_reps) {
    gate <- FALSE
    reason <- "insufficient bounded draws"
  } else if (gate && frac < stability) {
    gate <- FALSE
    reason <- "boundedness unstable across draws"
  } else if (gate && (!is.finite(se) || se <= 0)) {
    gate <- FALSE
    reason <- "degenerate endpoint scale"
  }
  z <- rep(NA_real_, length(vals))
  if (isTRUE(gate)) {
    z[ok] <- inward_sign * (vals[ok] - anchor) / se
  }
  list(
    ok = ok, n_ok = sum(ok), frac = frac, se = se, gate = isTRUE(gate),
    z = z, reason = reason
  )
}

# Envelope for one coefficient at one tau. The full-sample per-side status decides
# the shape (two-sided / one-sided upper / one-sided lower / unbounded / suppress).
logvar_endpoint_envelope_row <- function(lo, up, lo_st, up_st, f, alpha,
                                         min_reps, stability) {
  lc <- logvar_side_stat(lo, lo_st, f$set_lower, 1, min_reps, stability)
  uc <- logvar_side_stat(up, up_st, f$set_upper, -1, min_reps, stability)
  ls <- f$lower_status
  us <- f$upper_status
  side <- "none"
  ci_lo <- NA_real_
  ci_up <- NA_real_
  cval <- NA_real_
  reason <- NA_character_
  if (identical(ls, "bounded") && identical(us, "bounded")) {
    if (lc$gate && uc$gate) {
      # both-bounded pool: the joint two-side critical value needs D_L/s_L
      # and D_U/s_U both defined on a draw; divergent draws stay counted in
      # frac above (never dropped from the gate), just excluded from this
      # root's pool.
      common <- lc$ok & uc$ok
      cval <- logvar_root_critical(pmax(0, lc$z, uc$z)[common], alpha)
      ci_lo <- f$set_lower - cval * lc$se
      ci_up <- f$set_upper + cval * uc$se
      side <- "two-sided"
      reason <- "reported"
    } else {
      reason <- if (!lc$gate) lc$reason else uc$reason
    }
  } else if (identical(ls, "unbounded") && identical(us, "bounded")) {
    if (uc$gate) {
      cval <- logvar_root_critical(pmax(0, uc$z)[uc$ok], alpha)
      ci_lo <- -Inf
      ci_up <- f$set_upper + cval * uc$se
      side <- "upper"
      reason <- "reported"
    } else {
      reason <- uc$reason
    }
  } else if (identical(ls, "bounded") && identical(us, "unbounded")) {
    if (lc$gate) {
      cval <- logvar_root_critical(pmax(0, lc$z)[lc$ok], alpha)
      ci_lo <- f$set_lower - cval * lc$se
      ci_up <- Inf
      side <- "lower"
      reason <- "reported"
    } else {
      reason <- lc$reason
    }
  } else if (identical(ls, "unbounded") && identical(us, "unbounded")) {
    reason <- "full-sample set unbounded on both sides"
  } else {
    reason <- "full-sample side not certified bounded (unreliable)"
  }
  data.frame(
    se_lower = lc$se, se_upper = uc$se,
    n_lower = lc$n_ok, n_upper = uc$n_ok,
    frac_lower = lc$frac, frac_upper = uc$frac,
    gate_lower = lc$gate, gate_upper = uc$gate,
    side = side, c_value = cval, ci_lower = ci_lo, ci_upper = ci_up,
    reason = reason, row.names = NULL, stringsAsFactors = FALSE
  )
}

logvar_endpoint_envelope <- function(draws, full, alpha = 0.10,
                                     min_reps = boot_min_reps(nrow(draws$lower)),
                                     stability = 0.85) {
  stopifnot(
    identical(dim(draws$lower), dim(draws$upper)),
    identical(dim(draws$lower_status), dim(draws$lower)),
    identical(dim(draws$upper_status), dim(draws$upper)),
    ncol(draws$lower) == nrow(full)
  )
  rows <- lapply(seq_len(nrow(full)), function(k) {
    r <- logvar_endpoint_envelope_row(
      draws$lower[, k], draws$upper[, k],
      draws$lower_status[, k], draws$upper_status[, k],
      full[k, ], alpha, min_reps, stability
    )
    cbind(coef = full$coef[k], r, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# One joint critical value: the max-root maximized over all coefficients' live
# (gate-passing) sides, on draws where every contributing side is bounded. Written
# to diagnostics so the report can state coefficientwise-vs-simultaneous coverage.
logvar_simultaneous_critical <- function(draws, full, alpha = 0.10,
                                         min_reps = boot_min_reps(nrow(draws$lower)),
                                         stability = 0.85) {
  b <- nrow(draws$lower)
  root <- rep(0, b)
  # same both/all-bounded pool logic as the two-sided branch above.
  common <- rep(TRUE, b)
  any_live <- FALSE
  for (k in seq_len(nrow(full))) {
    f <- full[k, ]
    lc <- logvar_side_stat(
      draws$lower[, k], draws$lower_status[, k],
      f$set_lower, 1, min_reps, stability
    )
    uc <- logvar_side_stat(
      draws$upper[, k], draws$upper_status[, k],
      f$set_upper, -1, min_reps, stability
    )
    if (identical(f$lower_status, "bounded") && lc$gate) {
      root <- pmax(root, lc$z)
      common <- common & lc$ok
      any_live <- TRUE
    }
    if (identical(f$upper_status, "bounded") && uc$gate) {
      root <- pmax(root, uc$z)
      common <- common & uc$ok
      any_live <- TRUE
    }
  }
  if (!any_live) {
    return(NA_real_)
  }
  logvar_root_critical(pmax(0, root)[common], alpha)
}
