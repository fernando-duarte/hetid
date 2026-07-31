# One shared bootstrap reference distribution for both identified-set inference
# targets, so the two panels of the main table become mutually comparable.
# Target S is containment, P{[L,U] subset C} >= 1-alpha; Target P is pointwise
# coverage uniform over the set, inf over phi_0 in [L,U] of P{phi_0 in C} >=
# 1-alpha. Both are conservative quantiles of functions of the SAME stored
# per-draw endpoints, so the endpoint dependence is carried by the joint
# bootstrap distribution and never has to be estimated -- which is what the
# superseded normal-theory path did by fitting a bivariate normal. Both panels
# and the sensitivity draws call these. Cells and gates live in
# endpoint_target_cells.R; consumed through it by
# scripts-paper/inference/run_bootstrap_stage.R.

paper_source_once(paper_path(
  "support", "inference_post", "identified_set_inference.R"
))

# Conservative (1-alpha) order statistic of n finite values: the
# ceil((n+1)(1-alpha))-th smallest, capped at the largest (Politis-Romano-Wolf).
root_critical <- function(root, alpha) {
  root <- root[is.finite(root)]
  if (!length(root)) {
    return(NA_real_)
  }
  k <- min(length(root), ceiling((length(root) + 1) * (1 - alpha)))
  sort(root, partial = k)[k]
}

# Per-side inward studentized deviations and the regularity gate. inward_sign is
# +1 for the lower side (z = (vals - anchor)/s) and -1 for the upper
# (z = (anchor - vals)/s), so a positive z means the draw's interval is NARROWER
# than the full-sample one on that side. The scale is the MAD of every draw
# bounded on THIS side, including draws the two-sided root pool excludes because
# the other side is not bounded; that asymmetry is deliberate and is what
# reproduces the published envelope. The gate needs a finite anchor, at least
# min_reps bounded draws, a bounded share over the NON-FAILED draws of at least
# stability (unbounded and unreliable draws stay in that denominator, failed
# draws do not), and a positive finite scale.
endpoint_side_stat <- function(vals, status, anchor, inward_sign, min_reps,
                               stability) {
  ok <- is.finite(vals) & status == PAPER_ENDPOINT_STATUS[["bounded"]]
  n_valid <- sum(status != PAPER_ENDPOINT_STATUS[["failed"]])
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
    ok = ok, n_ok = sum(ok), n_valid = n_valid, frac = frac, se = se,
    gate = isTRUE(gate), z = z, reason = reason
  )
}

# Target S. Containment fails when either padded endpoint has still not cleared
# its population counterpart, so the root is the max of the inward deviations
# and the width never enters: how far apart the endpoints are is irrelevant to
# clearing each of them. The max with zero is what makes this a valid root of a
# one-sided failure probability, and it gives the distribution an atom at zero
# contributed by draws whose interval is wider on every live side. `...` carries
# the live sides' z vectors, so a half-infinite cell passes only its live side.
target_s_critical <- function(pool, alpha, ...) {
  root_critical(do.call(pmax, c(list(0), list(...)))[pool], alpha)
}

# Target P, over the continuum of truth positions phi_0 = L + lambda * w. Each
# side earns a WIDTH CREDIT -- truth sitting away from an endpoint leaves that
# endpoint room to spare before it can fail -- so the per-draw root is
#   max{0, z_l - lambda * d_l, z_u - (1 - lambda) * d_u},   d = w_hat / s,
# a max of three affine functions of lambda with slopes 0, -d_l and +d_u. Each
# is L-Lipschitz with L = max(d_l, d_u), and the order statistic INHERITS L,
# because order statistics are monotone in each argument and translation
# equivariant: f_b(lambda') <= f_b(lambda) + L*delta for every draw implies the
# same for the k-th smallest. That licenses a certified branch and bound, since
# on any [a,b] the two one-sided Lipschitz bounds average to
#   U[a,b] = (g(a) + g(b) + L * (b - a)) / 2 >= sup over [a,b] of g.
# The supremum genuinely needs this: an interior lambda attains it in about a
# third of the table's cells, so an endpoint search or a fixed grid is wrong.
#
# The pool is fixed once, and every root is finite on it, so n never changes and
# the monotonicity the Lipschitz argument needs holds. Ties break on the
# smallest left endpoint, so reruns are bit-identical.
#
# The stopping test is the gap on the REPORTED value min(c_s, max U), not on
# max U. U - M shrinks only as L*(b-a)/2, so a flat credited quantile would
# otherwise force uniform refinement to width 2*tolerance/L -- measured at tens
# of thousands of evaluations by L = 3 -- while the c_s cap that the ordering
# identity already licenses ends exactly those cases at once. Cells whose
# supremum is interior are unaffected: both tests agree there.
target_p_critical <- function(z_lower, z_upper, pool, d_lower, d_upper,
                              alpha, tolerance, c_s) {
  lipschitz <- max(d_lower, d_upper)
  if (!is.finite(lipschitz) || lipschitz <= 0) {
    return(list(
      c_p_lower = c_s, c_p_upper = c_s, evals = 0L, best_lambda = 0,
      interior = FALSE
    ))
  }
  z_lo <- z_lower[pool]
  z_up <- z_upper[pool]
  g <- function(lambda) {
    root_critical(
      pmax(0, z_lo - lambda * d_lower, z_up - (1 - lambda) * d_upper),
      alpha
    )
  }
  left <- 0
  right <- 1
  g_left <- g(0)
  g_right <- g(1)
  evals <- 2L
  best <- max(g_left, g_right)
  endpoint_best <- best
  best_lambda <- if (g_left >= g_right) 0 else 1
  bound <- function() (g_left + g_right + lipschitz * (right - left)) / 2
  repeat {
    upper <- bound()
    top <- max(upper)
    if (min(c_s, top) - best <= tolerance) {
      break
    }
    at <- which(upper == top)
    at <- at[[which.min(left[at])]]
    mid <- (left[[at]] + right[[at]]) / 2
    g_mid <- g(mid)
    evals <- evals + 1L
    if (g_mid > best) {
      best <- g_mid
      best_lambda <- mid
    }
    left <- c(left[-at], left[[at]], mid)
    right <- c(right[-at], mid, right[[at]])
    g_left <- c(g_left[-at], g_left[[at]], g_mid)
    g_right <- c(g_right[-at], g_mid, g_right[[at]])
  }
  # `top` is the bound the stopping test just accepted, and nothing mutates the
  # interval arrays between that test and this return, so reusing it is the same
  # number as recomputing the bound and does not ask a reader to prove they agree
  list(
    c_p_lower = best, c_p_upper = min(c_s, top), evals = evals,
    best_lambda = best_lambda, interior = best > endpoint_best + tolerance
  )
}
