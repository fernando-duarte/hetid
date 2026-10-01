bootstrap_root_critical <- function(root, alpha) {
  bootstrap_finite_arithmetic(root, "bootstrap root")
  if (!length(root)) {
    return(NA_real_)
  }
  k <- bootstrap_root_rank(length(root), alpha)
  sort(root, partial = k)[k]
}

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
    # Lower endpoints use +1 and upper endpoints -1 so inward movement is positive
    side$z[side$ok] <- inward_sign * (vals[side$ok] - anchor) / side$se
  }
  side
}

bootstrap_containment_critical <- function(pool, alpha, ...) {
  roots <- lapply(list(...), function(z) z[pool])
  bootstrap_finite_arithmetic(unlist(roots), "endpoint deviations")
  bootstrap_root_critical(do.call(pmax, c(list(0), roots)), alpha)
}
