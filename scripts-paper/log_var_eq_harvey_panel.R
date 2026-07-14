# Harvey robustness panel appended to the combined log-variance estimator
# panels (log_var_eq_ppml_table.R). Mirrors the PPML panel construction in
# that host exactly -- same fmt/set_cell/interleave cell formatters, same
# build_simple_latex_table invocation, same rule_after and fontsize -- and
# wraps the result in the creation-time LaTeX comment markers so later plans
# can append and diff blocks. The appender only appends: it never touches or
# reorders the log-OLS/PPML pair, so Harvey never influences the headline
# swap. Definitions only; sourced by log_var_eq_ppml_table.R, which supplies
# the shared cell formatters (log_var_eq_table_utils.R) and latex builders.

source("scripts-paper/log_var_eq_harvey_notes.R")

# Build the Harvey panel fragment: reference and Lewbel-point columns plus the
# per-display-tau hulls, t-statistic slots blank by construction, R^2 not
# defined for the variance MLE. Row labels come from the fitted coefficient
# vector (length minus one intercept), never a run_all global.
logvar_harvey_build_fragment <- function(harvey, n_obs, tau_display) {
  tab <- harvey$table
  n_pc_r <- length(tab$coef) - 1L
  sets <- harvey$sets[sprintf("%.17g", tau_display)]
  stopifnot(!any(vapply(sets, is.null, logical(1))))
  labels <- c(
    "$\\theta^{H}_0$", sprintf("$\\theta^{H}_{%d,R}$", seq_len(n_pc_r))
  )
  rows <- c(interleave(labels, ""), "$R^2$", "$N$")
  cols <- c(
    list(
      c(interleave(fmt(tab$reference), ""), "--", sprintf("%d", n_obs)),
      c(interleave(fmt(tab$point), ""), "--", sprintf("%d", n_obs))
    ),
    unname(lapply(sets, function(st) {
      stopifnot(identical(st$coef, tab$coef))
      c(
        interleave(set_cell(st$set_lower, st$set_upper, st$status), ""),
        "--", sprintf("%d", n_obs)
      )
    }))
  )
  build_simple_latex_table(
    rows, cols,
    col_headers = c(
      "Reference", "$\\tau{=}0$", sprintf("$\\tau{=}%.2g$", tau_display)
    ),
    caption = paste(
      "Harvey panel: $\\theta^{H}$, the Gaussian multiplicative-variance map",
      "(fixed robustness panel)."
    ),
    label = "tab:log_var_eq_panel_harvey",
    fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
    rule_after = 2L
  )
}

# Append the marker-wrapped Harvey panel after the incoming (already ordered)
# panel lines. The splice mirrors the host's local panel_block helper in shape;
# kept local so it never shadows that global helper. Returns the incoming lines
# unchanged with the Harvey block appended.
logvar_harvey_append_panel <- function(panels_lines, harvey, n_obs,
                                       tau_display, tau_baseline,
                                       grid_cap, fit_budget) {
  splice_block <- function(fragment, notes_lines) {
    cut <- match("\\end{threeparttable}", fragment)
    stopifnot(!is.na(cut))
    notes_block <- c(
      "% BEGIN LOGVAR NOTES harvey",
      "\\begin{tablenotes}[flushleft]",
      "\\scriptsize",
      paste0("\\item ", notes_lines),
      "\\end{tablenotes}",
      "% END LOGVAR NOTES harvey"
    )
    c(
      "% BEGIN LOGVAR PANEL harvey",
      fragment[seq_len(cut - 1L)], notes_block, fragment[cut:length(fragment)],
      "% END LOGVAR PANEL harvey"
    )
  }
  harvey_fragment <- logvar_harvey_build_fragment(harvey, n_obs, tau_display)
  harvey_notes <- build_harvey_panel_notes(
    harvey, tau_baseline, grid_cap, fit_budget
  )
  c(panels_lines, splice_block(harvey_fragment, harvey_notes))
}
