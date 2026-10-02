# Estimator-neutral batch scanner for closed-form coefficient maps over the
# feasible grid: running per-coefficient extremes with their arg-extreme
# points, scanned in chunks. coef_at(b) returns list(coef = p x k, eligible =
# logical k); ineligible columns count as fit failures, NaN values are
# missing, and +/-Inf values are kept (divergent sides are the caller's
# bookkeeping). residuals_at(b), when given, feeds the both-signs crossing
# tracker (an exact zero counts as both signs). pool_k > 1 also returns
# separated extra polish starts per coefficient and side from the engine's own
# pool builder, with the generic path's separation rule. Definitions only;
# sourced by residual_map.R.

logvar_projection_scan <- function(b_feas, coef_at, n_coef,
                                   chunk = LOGVAR_SEARCH_CONTROL$scan_chunk_size,
                                   residuals_at = NULL, pool_k = 1L) {
  stopifnot(nrow(b_feas) > 0L)
  best_min <- rep(Inf, n_coef)
  best_max <- rep(-Inf, n_coef)
  arg_min <- arg_max <- matrix(NA_real_, n_coef, ncol(b_feas))
  n_fail <- 0L
  any_nonpos <- any_nonneg <- FALSE
  all_vals <- if (pool_k > 1L) matrix(NA_real_, nrow(b_feas), n_coef)
  for (s in seq(1L, nrow(b_feas), by = chunk)) {
    rows <- s:min(s + chunk - 1L, nrow(b_feas))
    b_rows <- b_feas[rows, , drop = FALSE]
    if (!is.null(residuals_at)) {
      # rowSums comparisons are the C-level form of the both-signs tracker
      eps <- residuals_at(b_rows)
      any_nonpos <- any_nonpos | (rowSums(eps <= 0) > 0)
      any_nonneg <- any_nonneg | (rowSums(eps >= 0) > 0)
    }
    batch <- coef_at(b_rows)
    n_fail <- n_fail + sum(!batch$eligible)
    th <- batch$coef
    th[, !batch$eligible] <- NA
    th[is.nan(th)] <- NA
    if (pool_k > 1L) all_vals[rows, ] <- t(th)
    for (j in seq_len(n_coef)) {
      k_min <- which.min(th[j, ])
      k_max <- which.max(th[j, ])
      if (length(k_min) && th[j, k_min] < best_min[j]) {
        best_min[j] <- th[j, k_min]
        arg_min[j, ] <- b_rows[k_min, ]
      }
      if (length(k_max) && th[j, k_max] > best_max[j]) {
        best_max[j] <- th[j, k_max]
        arg_max[j, ] <- b_rows[k_max, ]
      }
    }
  }
  out <- list(
    min = best_min, max = best_max, arg_min = arg_min, arg_max = arg_max,
    n_fail = n_fail,
    cross_grid = if (is.null(residuals_at)) {
      integer(0)
    } else {
      which(any_nonpos & any_nonneg)
    }
  )
  if (pool_k > 1L) {
    span <- apply(b_feas, 2L, max) - apply(b_feas, 2L, min)
    pools <- logvar_engine_scan_pools(
      all_vals, lapply(seq_len(nrow(b_feas)), function(i) b_feas[i, ]),
      pool_k, LOGVAR_SEARCH_CONTROL$start_separation_fraction * sqrt(sum(span^2))
    )
    out <- c(out, pools)
  }
  out
}
