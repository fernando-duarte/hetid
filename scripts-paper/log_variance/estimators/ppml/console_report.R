# Console summary for the PPML log-variance driver, split from
# run_sets.R for the repository line cap. Reads the assembled
# log_var_eq_ppml plus the benchmark crossing counts; prints the per-tau coef
# hulls with each tau's attempted/evaluated/cached/failed budget counts, the
# three fragility diagnostics (min feasible |eps|, benchmark divergence
# directions, kappa at the Lewbel-point fit), and the editorial ordering
# verdict from an exact numeric-tau lookup. Sourced by run_sets.R
# before its cleanup; prints only, defines no persistent object.

paper_source_once(paper_path(
  "log_variance", "tables", "console_formatting.R"
))

ppml_taus <- set_id_mean_eq$tau_display
logvar_print_map_summary(
  "PPML log-variance map",
  log_var_eq_ppml,
  ppml_taus
)

# the three fragility diagnostics accompanying the ordering rule
cat("  fragility diagnostics:\n")
cat(sprintf(
  "    min feasible |eps| by tau: %s\n",
  paste(
    paste0(
      paper_format_general(
        ppml_taus,
        PAPER_REPORTING_CONTROL$precision$tau_significant
      ),
      "=",
      paper_format_general(
        log_var_eq_ppml$min_feasible_abs_eps,
        PAPER_REPORTING_CONTROL$precision$console_significant
      )
    ),
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
ord <- logvar_panel_order(
  log_var_eq$n_cross,
  set_id_mean_eq$tau_display,
  log_var_eq$tau_baseline
)
n_base <- log_var_eq$n_cross[[
  which(set_id_mean_eq$tau_display == log_var_eq$tau_baseline)
]]
cat(sprintf(
  "  ordering: n_cross[tau = %s] = %d %s 0 -> %s panel first\n",
  format(log_var_eq$tau_baseline),
  n_base,
  if (n_base > 0) ">" else "<=",
  ord[1]
))

rm(ppml_taus, div_dirs, ord, n_base)
