bootstrap_root_rank <- function(n, alpha) {
  if (n == 0L) {
    return(NA_real_)
  }
  min(n, ceiling((n + 1) * (1 - alpha)))
}

# A diagnostic for the explicitly listed live sides, not an all-coefficient CI
bootstrap_simultaneous_diagnostic <- function(sides, full, alpha, min_reps) {
  n <- length(sides[[1]]$lower$ok)
  root <- rep(0, n)
  common <- rep(TRUE, n)
  nonfinite <- rep(FALSE, n)
  active <- matrix(FALSE, nrow(full), 2L, dimnames = list(full$coef, c("lower", "upper")))
  for (k in seq_len(nrow(full))) {
    for (side in colnames(active)) {
      x <- sides[[k]][[side]]
      if (full[[paste0(side, "_status")]][k] == "bounded" && x$gate) {
        root <- pmax(root, x$z)
        common <- common & x$ok
        nonfinite <- nonfinite | (x$ok & !is.finite(x$z))
        active[k, side] <- TRUE
      }
    }
  }
  n_common <- if (any(active)) sum(common) else 0L
  reason <- if (!any(active)) {
    "no_active_sides"
  } else if (n_common == 0L) {
    "no_common_draws"
  } else if (any(nonfinite[common])) {
    "nonfinite_endpoint_deviations"
  } else {
    "reported"
  }
  list(
    critical = if (reason == "reported") {
      bootstrap_root_critical(root[common], alpha)
    } else {
      NA_real_
    },
    reason = reason,
    n_common = n_common, active_sides = active,
    meets_min_reps = n_common >= min_reps, root_rank = bootstrap_root_rank(n_common, alpha),
    tail_resolution = 1 / (n_common + 1)
  )
}
