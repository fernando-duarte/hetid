# Conservative (1-alpha) order statistic of n finite values: the
# ceil((n+1)(1-alpha))-th smallest, capped at the largest (Politis-Romano-Wolf).
bootstrap_root_critical <- function(root, alpha) {
  bootstrap_finite_arithmetic(root, "bootstrap root")
  if (!length(root)) {
    return(NA_real_)
  }
  k <- bootstrap_root_rank(length(root), alpha)
  sort(root, partial = k)[k]
}

# Per-side inward studentized deviations and the regularity gate. inward_sign is
# +1 for the lower side (z = (vals - anchor)/s) and -1 for the upper
# (z = (anchor - vals)/s), so a positive z means the draw's interval is NARROWER
# than the full-sample one on that side. The scale is the MAD of every draw
# bounded on this side, including draws the two-sided root pool excludes because
# the other side is not bounded; that asymmetry is deliberate and is what
# reproduces the published envelope. The gate needs a finite anchor, at least
# min_reps bounded draws, a bounded share over the non-failed draws of at least
# stability (unbounded and unreliable draws stay in that denominator, failed
# draws do not), and a positive finite scale.
bootstrap_endpoint_gate <- function(vals, status, anchor, min_reps, stability) {
  ok <- is.finite(vals) & status == "bounded"
  n_valid <- sum(status != "failed")
  frac <- if (n_valid > 0L) sum(ok) / n_valid else 0
  se <- if (sum(ok) >= 2L) stats::mad(vals[ok]) else NA_real_
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
  list(
    ok = ok, n_ok = sum(ok), n_valid = n_valid, frac = frac, se = se,
    gate = isTRUE(gate), reason = reason
  )
}

bootstrap_endpoint_side <- function(vals, status, anchor, inward_sign, min_reps, stability) {
  side <- bootstrap_endpoint_gate(vals, status, anchor, min_reps, stability)
  side$z <- rep(NA_real_, length(vals))
  if (side$gate) {
    # Check finiteness when a consumer chooses its fixed root pool. An unused
    # side or an unavailable diagnostic must not abort another coefficient.
    side$z[side$ok] <- inward_sign * (vals[side$ok] - anchor) / side$se
  }
  side
}

# Target S. Containment fails when either padded endpoint has still not cleared
# its population counterpart, so the root is the max of the inward deviations
# and the width never enters: how far apart the endpoints are is irrelevant to
# clearing each of them. The max with zero is what makes this a valid root of a
# one-sided failure probability, and it gives the distribution an atom at zero
# contributed by draws whose interval is wider on every live side. `...` carries
# the live sides' z vectors, so a half-infinite cell passes only its live side.
bootstrap_containment_critical <- function(pool, alpha, ...) {
  roots <- lapply(list(...), function(z) z[pool])
  bootstrap_finite_arithmetic(unlist(roots), "endpoint deviations")
  bootstrap_root_critical(do.call(pmax, c(list(0), roots)), alpha)
}
