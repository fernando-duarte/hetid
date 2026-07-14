# Console summary for the PPML log-variance driver, split from
# log_var_eq_ppml_sets.R for the repository line cap. Reads the assembled
# log_var_eq_ppml plus the benchmark crossing counts; prints the per-tau coef
# hulls with each tau's attempted/evaluated/cached/failed budget counts, the
# three fragility diagnostics (min feasible |eps|, benchmark divergence
# directions, kappa at the Lewbel-point fit), and the editorial ordering
# verdict from an exact numeric-tau lookup. Sourced by log_var_eq_ppml_sets.R
# before its cleanup; prints only, defines no persistent object.

cat(sprintf(
  "PPML log-variance map: N = %d over %s to %s\n",
  log_var_eq_ppml$sample$n,
  format(log_var_eq_ppml$sample$span[1]),
  format(log_var_eq_ppml$sample$span[2])
))

# per display tau: the five coefficient hulls (or side statuses) and the tau's
# budget counters
ppml_tau_names <- names(log_var_eq_ppml$sets)
ppml_taus <- set_id_mean_eq$tau_display
for (i in seq_along(ppml_tau_names)) {
  nm <- ppml_tau_names[i]
  tb <- log_var_eq_ppml$sets[[nm]]
  d <- log_var_eq_ppml$counts[[nm]]
  hull <- vapply(seq_len(nrow(tb)), function(j) {
    if (identical(tb$status[j], "bounded")) {
      sprintf("[%.3g,%.3g]", tb$set_lower[j], tb$set_upper[j])
    } else {
      tb$status[j]
    }
  }, character(1))
  cat(sprintf(
    "  tau = %.2g: %s | attempted %d evaluated %d cached %d failed %d\n",
    ppml_taus[i], paste(hull, collapse = " "),
    d$n_attempted, d$n_evaluated, d$n_cached, d$n_failed
  ))
}

# the three fragility diagnostics accompanying the ordering rule
cat("  fragility diagnostics:\n")
cat(sprintf(
  "    min feasible |eps| by tau: %s\n",
  paste(
    sprintf("%.2g=%.3g", ppml_taus, log_var_eq_ppml$min_feasible_abs_eps),
    collapse = " "
  )
))
div_dirs <- vapply(names(log_var_eq_ppml$benchmark_divergence), function(nm) {
  b <- log_var_eq_ppml$benchmark_divergence[[nm]]
  sprintf("%s(lo %d,hi %d)", nm, sum(b$lower_unbounded), sum(b$upper_unbounded))
}, character(1))
cat(sprintf(
  "    benchmark divergent sides by tau: %s\n",
  paste(div_dirs, collapse = " ")
))
cat(sprintf(
  "    kappa(X' diag(mu) X) at the Lewbel-point fit: %.3g\n",
  log_var_eq_ppml$cond_weighted_xx
))

# editorial ordering verdict: PPML first when the benchmark's tau = 0.05
# crossing count is positive, by exact numeric-tau lookup
ord <- logvar_panel_order(log_var_eq$n_cross, set_id_mean_eq$tau_display)
n_base <- log_var_eq$n_cross[[which(set_id_mean_eq$tau_display == 0.05)]]
cat(sprintf(
  "  ordering: n_cross[tau = 0.05] = %d %s 0 -> %s panel first\n",
  n_base, if (n_base > 0) ">" else "<=", ord[1]
))

rm(ppml_tau_names, ppml_taus, i, nm, tb, d, hull, div_dirs, ord, n_base)
