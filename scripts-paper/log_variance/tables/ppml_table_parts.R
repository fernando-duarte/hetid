# PPML table-part assembly and shared standard-error note.

# The point-column conditioning caveat shared verbatim by both estimators' SE
# notes: tau = 0 conditions on the plug-in news vector, while the set columns
# carry either a separate endpoint envelope or an explicit deferral.
logvar_se_note_caveat <- function(set_endpoint_inference = FALSE) {
  prefix <- paste(
    "The $\\tau{=}0$ statistics condition on the plug-in Lewbel news vector",
    "$b_N$ and do not propagate its first-stage sampling error; $\\tau{>}0$",
    "set columns are identified-set ranges, not point estimates."
  )
  if (isTRUE(set_endpoint_inference)) {
    return(paste(
      prefix,
      "Their moving-block-bootstrap outer confidence envelopes are reported",
      "separately beneath the set cells."
    ))
  }
  paste(
    prefix,
    "No standard error is attached; moving-block-bootstrap set-endpoint",
    "uncertainty is deferred."
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
