# Harvey robustness panel appended to the combined log-variance estimator
# panels (render_panels.R). Mirrors the PPML panel construction in
# that host exactly -- same fmt/set_cell/interleave cell formatters, same
# build_simple_latex_table invocation, same rule_after and fontsize -- and
# wraps the result in creation-time LaTeX comment markers for stable appends and
# diffs. The appender only appends: it never touches or
# reorders the log-OLS/PPML pair, so Harvey never influences the headline
# swap. Definitions only; callers supply the shared cell formatters
# (table_formatting.R) and LaTeX builders.

paper_source_once(paper_path("log_variance", "tables", "harvey_caption.R"))

# Build the Harvey panel fragment: reference and Lewbel-point columns plus the
# per-display-tau hulls, R^2 not defined for the variance MLE. Row labels come
# from the fitted coefficient vector (length minus one intercept), never a
# runner global. The point columns render t-statistics/stars when se_type is
# set (via the shared logvar_se_point_col); stat slots stay blank by
# construction when se_type is NULL (back-compat). envelope threads a per-tau
# (paper_tau_key-keyed) confidence-envelope frame (log_var_eq_set_boot
# $harvey) the same way logvar_ppml_table_parts does: NULL (the default) keeps
# every column byte-identical to the pre-envelope renderer.
logvar_harvey_build_fragment <- function(harvey, n_obs, tau_display,
                                         caption = NULL, label = NULL,
                                         se_type = NULL, envelope = NULL) {
  if (is.null(caption)) {
    caption <- paste(
      "Harvey panel: $\\theta^{H}$, the Gaussian multiplicative-variance map",
      "(fixed robustness panel)."
    )
  }
  if (is.null(label)) {
    label <- artifact_latex_label(
      "log_variance_panels_table",
      "harvey"
    )
  }
  logvar_estimator_panel_fragment(
    harvey,
    n_obs,
    tau_display,
    list(
      intercept_label = "$\\theta^{H}_0$",
      slope_template = "$\\theta^{H}_{%d,R}$",
      reference_header = "Reference"
    ),
    caption,
    label,
    se_type = se_type,
    se_types = LOGVAR_HARVEY_SE_TYPES,
    envelope = envelope
  )
}

# Append the marker-wrapped Harvey panel after the incoming (already ordered)
# panel lines. The splice mirrors the host's local panel_block helper in shape;
# kept local so it never shadows that global helper. Returns the incoming lines
# unchanged with the Harvey block appended.
logvar_harvey_append_panel <- function(panels_lines, harvey, n_obs,
                                       tau_display, tau_baseline,
                                       grid_cap, fit_budget, caption = NULL,
                                       label = NULL, include_ordering = TRUE,
                                       se_type = NULL, se_hac_lags = NULL,
                                       set_endpoint_inference = FALSE) {
  harvey_fragment <- logvar_harvey_build_fragment(
    harvey, n_obs, tau_display, caption, label, se_type
  )
  harvey_notes <- build_harvey_panel_notes(
    harvey, tau_baseline, grid_cap, fit_budget,
    se_type = se_type,
    se_hac_lags = se_hac_lags,
    set_endpoint_inference = set_endpoint_inference,
    include_ordering = include_ordering
  )
  logvar_append_panel(
    panels_lines,
    harvey_fragment,
    harvey_notes,
    "harvey"
  )
}
