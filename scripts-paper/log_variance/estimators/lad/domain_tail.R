# M-scale tail classifier for the median (LAD) log-variance estimator: reads a
# per-path probe trace in the coordinate M = -2 log(|e_i,s| / e_scale_ref) and
# returns per-coefficient operational evidence, not an LP recession certificate.
# Sibling of crossing_domain.R (which sources this file). The predicates
# (stable_finite / persistent_divergent_evidence / unresolved, plus uninformative
# for a probe too thin to hold any window) and the eligibility gate follow the
# plan's crossing-classification contract; uninformative splits the no-evidence
# case off "unresolved" so a degenerate probe cannot fail a coefficient closed.

# OLS slope and R^2 of y on a single regressor x with intercept. A constant x or
# a constant y leaves the slope at zero and reports R^2 = 1 (only consulted in the
# divergent branch, which additionally requires a nonzero slope, so the value is
# inert whenever y does not move).
.lad_tail_ols <- function(x, y) {
  xb <- mean(x)
  yb <- mean(y)
  sxx <- sum((x - xb)^2)
  sxy <- sum((x - xb) * (y - yb))
  syy <- sum((y - yb)^2)
  slope <- if (sxx > 0) sxy / sxx else 0
  r2 <- if (sxx > 0 && syy > 0) (sxy^2) / (sxx * syy) else 1
  list(slope = slope, r2 = r2)
}

# Eligibility of a tail window (row indices into the trace): strictly increasing
# finite M, finite coefficient values, every fit ok, and one unchanged active
# basis / residual-sign signature. Failing the gate is unresolved before any
# slope rule is considered.
.lad_window_ok <- function(idx, m, cvals, fit_status, sig) {
  all(is.finite(m[idx])) && all(is.finite(cvals[idx])) &&
    all(diff(m[idx]) > 0) && all(fit_status[idx] == "ok") &&
    length(unique(sig[idx])) == 1L
}

# Classify one coefficient column. Order matters: stable is tested first on the
# six-point window, divergence next on the eight-point window, else unresolved.
# A trace too short to hold even the six-point window is uninformative (a thin or
# near-tangent probe that yielded no tail evidence either way), which the caller
# skips -- it is missing data, not the genuine ambiguity "unresolved" fails closed
# on. An eligible-but-inconclusive window (present but neither stable nor
# divergent) still returns "unresolved".
.lad_classify_one <- function(
  cvals,
  m,
  fit_status,
  sig,
  control
) {
  n <- length(m)
  stable_window <- control$tail_stable_window
  divergent_window <- control$tail_divergent_window
  out <- function(status, endpoint = NA_character_, extra = list()) {
    c(list(status = status, endpoint = endpoint), extra)
  }
  if (n < stable_window) {
    return(out("uninformative"))
  }
  if (n >= stable_window) {
    i6 <- (n - stable_window + 1L):n
    if (.lad_window_ok(i6, m, cvals, fit_status, sig)) {
      c6 <- cvals[i6]
      m6 <- m[i6]
      stol <- control$tail_stability_rtol *
        max(1, max(abs(c6)))
      last4 <- utils::tail(
        diff(c6),
        control$tail_increment_count
      )
      f6 <- .lad_tail_ols(m6, c6)
      move6 <- abs(f6$slope) * diff(range(m6))
      if (all(abs(last4) <= stol) && move6 < stol) {
        return(out("stable_finite", extra = list(
          slope6 = f6$slope, m_span = diff(range(m6)), implied_move = move6
        )))
      }
    }
  }
  if (n >= divergent_window) {
    i8 <- (n - divergent_window + 1L):n
    i6 <- (n - stable_window + 1L):n
    if (.lad_window_ok(i8, m, cvals, fit_status, sig)) {
      c8 <- cvals[i8]
      m8 <- m[i8]
      c6 <- cvals[i6]
      m6 <- m[i6]
      f6 <- .lad_tail_ols(m6, c6)
      f8 <- .lad_tail_ols(m8, c8)
      stol <- control$tail_stability_rtol *
        max(1, max(abs(c8)))
      denom <- max(
        abs(f6$slope),
        abs(f8$slope),
        control$tail_slope_floor
      )
      same_sign <- f6$slope != 0 && sign(f6$slope) == sign(f8$slope)
      slope_stab <- abs(f6$slope - f8$slope) / denom <=
        control$tail_slope_agreement_rtol
      mono <- all(sign(diff(c6)) == sign(f6$slope))
      move6 <- abs(f6$slope) * diff(range(m6))
      move_ok <-
        move6 > control$tail_move_tolerance_multiplier * stol &&
          move6 > control$tail_relative_move_floor *
            max(1, stats::median(abs(c8)))
      if (
        diff(range(m8)) >= control$tail_min_m_span &&
          same_sign &&
          slope_stab &&
          f8$r2 >= control$tail_min_r_squared &&
          mono &&
          move_ok
      ) {
        return(out("persistent_divergent_evidence",
          endpoint = if (f6$slope > 0) "upper" else "lower",
          extra = list(
            slope6 = f6$slope, slope8 = f8$slope, r2 = f8$r2,
            m_span = diff(range(m8)), implied_move = move6
          )
        ))
      }
    }
  }
  out("unresolved")
}

# Public entry: trace = list(m increasing, coef matrix [one column per
# coefficient], fit_status, active_signature). Returns list(coef = per-coefficient
# list(status, endpoint, metrics)). Missing status/signature default to a single
# ok basis so a bare (m, coef) trace still classifies.
logvar_lad_tail_classify <- function(
  trace,
  control = LOGVAR_LAD_CONTROL
) {
  m <- trace$m
  coef <- trace$coef
  if (is.null(dim(coef))) coef <- matrix(coef, ncol = 1L)
  n <- length(m)
  fit_status <- if (is.null(trace$fit_status)) rep("ok", n) else trace$fit_status
  sig <- if (is.null(trace$active_signature)) rep("s", n) else trace$active_signature
  fit_status <- rep_len(as.character(fit_status), n)
  sig <- rep_len(as.character(sig), n)
  per <- lapply(seq_len(ncol(coef)), function(j) {
    .lad_classify_one(
      coef[, j],
      m,
      fit_status,
      sig,
      control
    )
  })
  names(per) <- colnames(coef)
  list(coef = per)
}
