bootstrap_pointwise_critical <- function(z_lower, z_upper, pool, d_lower, d_upper,
                                         alpha, tolerance, c_s, max_evals =
                                           BOOTSTRAP_INFERENCE_DEFAULTS$MAX_EVALS) {
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
  # With a fixed draw pool, the largest absolute root slope also bounds quantile changes
  # Combining the endpoint bounds gives an upper bound throughout each interval
  bound <- function() {
    bootstrap_finite_arithmetic(
      (g_left + g_right + lipschitz * (right - left)) / 2, "search upper bound"
    )
  }
  repeat {
    upper <- bound()
    top <- max(upper)
    # Credits only reduce roots, so c_s also bounds the quantile and can end refinement early
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
  # Every stop occurs before interval updates, so top remains the final search bound
  list(
    c_p_lower = best, c_p_upper = max(best, min(c_s, top)), evals = evals,
    best_lambda = best_lambda, interior = best > endpoint_best + tolerance,
    search_stop = search_stop
  )
}
