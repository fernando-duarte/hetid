# Frozen verbatim-logic copy of the log-OLS driver closure logvar_set_at_tau,
# the reference the engine's benchmark configuration is checked against.
# Sourced by oracle_checks.R.

# Frozen copy of logvar_set_at_tau from the log-OLS runner:
# tau_quadratic_
# system becomes the supplied qs and the driver globals become parameters; every
# other line keeps the driver's logic unchanged.
oracle_set_at_tau <- function(qs, b_tab, w1, w2, proj, b_point, grid_n,
                              grid_floor, qtr) {
  logvar_coefs <- rownames(proj)
  stopifnot(identical(b_tab$coef, colnames(w2)))
  na_table <- function(status) {
    data.frame(
      coef = logvar_coefs, set_lower = NA_real_, set_upper = NA_real_,
      status = status, row.names = NULL
    )
  }
  out <- function(table, n_cross = NA_integer_, n_feasible = NA_integer_,
                  cross_qtr = NULL) {
    list(table = table, n_cross = n_cross, n_feasible = n_feasible, cross_qtr = cross_qtr)
  }
  if (any(b_tab$status != "bounded")) {
    status <- if (any(b_tab$status == "unbounded")) "unbounded" else "unreliable"
    return(out(na_table(status)))
  }
  census <- logvar_crossing_census(qs, b_tab$set_lower, b_tab$set_upper, w1, w2)
  if (length(census$unresolved) > 0L) {
    return(out(na_table("unreliable"), n_cross = length(census$cross)))
  }
  b_feas <- logvar_feasible_grid(qs, b_tab$set_lower, b_tab$set_upper, grid_n)
  if (nrow(b_feas) < grid_floor) {
    b_feas <- logvar_feasible_grid(qs, b_tab$set_lower, b_tab$set_upper, 2L * grid_n - 1L)
  }
  if (nrow(b_feas) == 0L) {
    return(out(na_table("unreliable"), n_cross = length(census$cross), n_feasible = 0L))
  }
  # kept verbatim on purpose: production shares this via quadratic_point_feasible,
  # and routing the oracle through it would make the equivalence checks vacuous
  if (!anyNA(b_point)) {
    if (.feasibility_residual(qs, b_point, rep(1, length(qs$A_i))) <= 0) {
      b_feas <- rbind(b_feas, b_point)
    }
  }
  scan <- logvar_grid_scan(b_feas, w1, w2, proj)
  cross_all <- sort(union(census$cross, scan$cross_grid))
  lower_unb <- apply(proj[, cross_all, drop = FALSE] > 0, 1, any)
  upper_unb <- apply(proj[, cross_all, drop = FALSE] < 0, 1, any)
  lower <- ifelse(lower_unb, -Inf, scan$min)
  upper <- ifelse(upper_unb, Inf, scan$max)
  unreliable <- rep(FALSE, length(logvar_coefs))
  for (j in seq_along(logvar_coefs)) {
    scan_j <- c(scan$min[j], scan$max[j])
    scale_j <- max(1, abs(scan_j[is.finite(scan_j)]))
    for (side in c("min", "max")) {
      if (if (side == "min") lower_unb[j] else upper_unb[j]) next
      starts <- list(if (side == "min") scan$arg_min[j, ] else scan$arg_max[j, ])
      if (!anyNA(b_point)) starts <- c(starts, list(b_point))
      accepted <- FALSE
      for (b_start in starts) {
        pol <- logvar_polish_bound(qs, side, b_start, scale_j, w1, w2, proj[j, ])
        if (pol$suspect) unreliable[j] <- TRUE
        if (is.null(pol$bound)) next
        accepted <- TRUE
        if (side == "min" && pol$bound < lower[j]) lower[j] <- pol$bound
        if (side == "max" && pol$bound > upper[j]) upper[j] <- pol$bound
      }
      if (!accepted) unreliable[j] <- TRUE
    }
  }
  status <- ifelse(unreliable, "unreliable",
    ifelse(lower_unb | upper_unb, "unbounded", "bounded")
  )
  out(
    data.frame(
      coef = logvar_coefs, set_lower = lower, set_upper = upper,
      status = status, row.names = NULL
    ),
    n_cross = length(cross_all), n_feasible = nrow(b_feas), cross_qtr = qtr[cross_all]
  )
}
