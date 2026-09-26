# Target P, over the continuum of truth positions phi_0 = L + lambda * w. Each
# side earns a width credit -- truth sitting away from an endpoint leaves that
# endpoint room to spare before it can fail -- so the per-draw root is
#   max{0, z_l - lambda * d_l, z_u - (1 - lambda) * d_u},   d = w_hat / s,
# a max of three affine functions of lambda with slopes 0, -d_l and +d_u. Each
# is L-Lipschitz with L = max(d_l, d_u), and the order statistic inherits L,
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
# The stopping test is the gap on the reported value min(c_s, max U), not on
# max U. U - M shrinks only as L*(b-a)/2, so a flat credited quantile would
# otherwise force uniform refinement to width 2*tolerance/L -- measured at tens
# of thousands of evaluations by L = 3 -- while the c_s cap that the ordering
# identity already licenses ends exactly those cases at once. Cells whose
# supremum is interior are unaffected: both tests agree there
bootstrap_pointwise_critical <- function(z_lower, z_upper, pool, d_lower, d_upper,
                                         alpha, tolerance, c_s, max_evals =
                                           BOOTSTRAP_INFERENCE_DEFAULTS$max_evals) {
  lipschitz <- max(d_lower, d_upper)
  bootstrap_finite_arithmetic(lipschitz, "width credit")
  if (lipschitz == 0) {
    return(list(
      c_p_lower = c_s, c_p_upper = c_s, evals = 0L, best_lambda = 0,
      interior = FALSE, search_stop = "zero_width"
    ))
  }
  z_lo <- z_lower[pool]
  z_up <- z_upper[pool]
  g <- function(lambda) {
    bootstrap_root_critical(
      pmax(0, z_lo - lambda * d_lower, z_up - (1 - lambda) * d_upper),
      alpha
    )
  }
  left <- 0
  right <- 1
  g_left <- g(0)
  g_right <- g(1)
  bootstrap_finite_arithmetic(c(g_left, g_right), "endpoint quantile")
  evals <- 2L
  best <- max(g_left, g_right)
  endpoint_best <- best
  best_lambda <- if (g_left >= g_right) 0 else 1
  bound <- function() {
    bootstrap_finite_arithmetic(
      (g_left + g_right + lipschitz * (right - left)) / 2, "search upper bound"
    )
  }
  repeat {
    upper <- bound()
    top <- max(upper)
    if (min(c_s, top) - best <= tolerance) {
      search_stop <- "tolerance"
      break
    }
    if (evals >= max_evals) {
      search_stop <- "max_evals"
      break
    }
    at <- which(upper == top)
    at <- at[[which.min(left[at])]]
    mid <- (left[[at]] + right[[at]]) / 2
    if (mid <= left[[at]] || mid >= right[[at]]) {
      search_stop <- "precision"
      break
    }
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
  # top is the accepted stopping bound; interval arrays are unchanged before return, so reusing it
  # equals recomputation and makes that equality explicit
  list(
    c_p_lower = best, c_p_upper = max(best, min(c_s, top)), evals = evals,
    best_lambda = best_lambda, interior = best > endpoint_best + tolerance,
    search_stop = search_stop
  )
}
