# PPML table-part assembly and shared standard-error note.

# The point-column caveat shared by both estimators' SE notes. The set-column
# sentence is common to both branches; the tau = 0 sentence is not, because the
# branch tracks whether the bootstrap objects were threaded into this table.
# With them, tau = 0 is a bootstrap t statistic that propagates the first-stage
# news-vector error; without them (the three tables that render before the
# bootstrap stage) it is the analytic statistic at the plug-in news vector.
logvar_se_note_caveat <- function(set_endpoint_inference = FALSE) {
  set_cols <- paste(
    "The $\\tau{>}0$ set columns are identified-set ranges, not point",
    "estimates."
  )
  if (isTRUE(set_endpoint_inference)) {
    return(paste(
      "The $\\tau{=}0$ statistics divide the point estimate by the robust scale",
      "of its bootstrap draws, which re-estimate the mean equation, so they",
      "propagate the first-stage sampling error in the Lewbel news vector",
      "$b_N$. The analytic statistic conditions on a fixed plug-in $b_N$",
      "instead, so the two are not comparable; it remains computed and is",
      "reported in the diagnostics.",
      set_cols,
      "Their moving-block bootstrap confidence intervals are reported beneath",
      "the set cells."
    ))
  }
  paste(
    "The $\\tau{=}0$ statistics condition on the plug-in Lewbel news vector",
    "$b_N$ and do not propagate its first-stage sampling error.",
    set_cols,
    "No standard error is attached to them; moving-block-bootstrap",
    "set-endpoint uncertainty is deferred."
  )
}

# Canonical PPML table parts: the quasi-Poisson reference and Lewbel-point
# columns followed by exact-keyed display-tau hulls. Both the primary table and
# the combined panels consume this one assembly path so their PPML cells cannot
# drift. The statistic slots and R-squared row are blank by construction, unless
# envelope supplies a per-tau (paper_tau_key-keyed) confidence-envelope
# frame (log_var_eq_set_boot$ppml), in which case the blank row beneath each set
# cell instead renders that tau's per-coef envelope_cell. NULL (the default)
# keeps every column byte-identical to the pre-envelope renderer.
paper_source_once(paper_path(
  "log_variance", "tables", "estimator_panel.R"
))

logvar_ppml_table_parts <- function(ppml, tau_display, n_pc_r, se_type = NULL,
                                    envelope = NULL) {
  model <- PAPER_ANALYSIS_CONTRACT$model
  expected_coef <- c(
    model$intercept_col,
    model$return_pc_cols[seq_len(n_pc_r)]
  )
  stopifnot(length(expected_coef) == n_pc_r + 1L)
  logvar_estimator_panel_parts(
    ppml,
    ppml$sample$n,
    tau_display,
    list(
      intercept_label = "$\\theta_0$",
      slope_template = "$\\theta_{%d,R}$",
      reference_header = "OLS",
      expected_coef = expected_coef
    ),
    se_type = se_type,
    se_types = LOGVAR_PPML_SE_TYPES,
    envelope = envelope
  )
}
