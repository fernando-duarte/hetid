# PPML table-part assembly and shared standard-error note.

# The point-column caveat shared by both estimators' SE notes. Every table now
# publishes after the bootstrap stage, so the tau = 0 sentence is the same in
# both branches. set_endpoint_inference records only whether this table carries
# tau > 0 interval rows, and decides nothing about the tau = 0 column.
logvar_se_note_caveat <- function(set_endpoint_inference = FALSE) {
  tau_zero <- paste(
    "The $\\tau{=}0$ statistics divide the point estimate by the robust scale",
    "of its bootstrap draws, which re-estimate the mean equation, so they",
    "propagate the first-stage sampling error in the Lewbel news vector",
    "$b_N$. The analytic statistic conditions on a fixed plug-in $b_N$",
    "instead, so the two are not comparable; it remains computed and is",
    "reported in the diagnostics."
  )
  set_cols <- paste(
    "The $\\tau{>}0$ set columns are identified-set ranges, not point",
    "estimates."
  )
  if (isTRUE(set_endpoint_inference)) {
    return(paste(
      tau_zero,
      set_cols,
      "Their moving-block bootstrap confidence intervals are reported beneath",
      "the set cells."
    ))
  }
  # This variant deliberately shows sets without intervals. It must not say the
  # endpoint inference is deferred: it exists, and the inference variant of this
  # table reports it. Saying otherwise tells a reader the work was never done.
  paste(
    tau_zero,
    set_cols,
    "No interval is reported beneath them in this variant; the moving-block",
    "bootstrap confidence intervals for the set endpoints are reported in the",
    "inference variant of this table."
  )
}

# Canonical PPML table parts: the quasi-Poisson reference and Lewbel-point
# columns followed by exact-keyed display-tau hulls. Both the primary table and
# the combined panels consume this one assembly path so their PPML cells cannot
# drift. The R-squared row is blank by construction, and so are the set-cell
# statistic slots unless envelope supplies a per-tau (paper_tau_key-keyed)
# confidence-envelope frame (log_var_eq_set_boot$ppml), in which case the blank
# row beneath each set cell instead renders that tau's per-coef envelope_cell.
# point_stat has no default on purpose. The notes these tables emit now assert
# unconditionally that the tau = 0 statistics propagate the first-stage error, so a
# table that omitted the frame would print an analytic ratio under a note claiming
# otherwise. Requiring it makes that omission impossible rather than merely
# unlikely; a caller genuinely wanting the analytic branch passes NULL and says so.
# point_stat supplies the tau = 0 column's bootstrap statistic frame and is
# independent of envelope. NULL (the default) keeps every column byte-identical
# to the pre-envelope renderer.
paper_source_once(paper_path(
  "log_variance", "tables", "estimator_panel.R"
))

logvar_ppml_table_parts <- function(ppml, tau_display, n_pc_r, se_type = NULL,
                                    envelope = NULL, point_stat) {
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
    envelope = envelope,
    point_stat = point_stat
  )
}
