# Panel fragment, marker-wrapped appender, and notes builder for the median (LAD)
# log-variance panel appended to the combined estimator panels
# (render_panels.R). Mirrors the Harvey panel/notes pair exactly -- same
# fmt/set_cell/interleave formatters, same build_simple_latex_table invocation,
# same rule_after and fontsize, same creation-time comment markers -- and only
# appends: the log-OLS/PPML/Harvey order is never touched. The reported cells are
# the attained punctured-domain hulls; the closure diagnostics live in the CSV and
# the notes, never in the panel. Definitions only; sourced by the panels table when
# log_var_eq_lad exists.

# The median panel fragment: reference and Lewbel-point columns plus the
# per-display-tau attained hulls, t slots blank by construction, R^2 undefined for
# a quantile fit. Row labels are the conditional-median coefficient vector, never a
# runner global; the intercept theta^0.5_0 is the median normalization, never
# shared with theta^log_0 / theta^var_0 / theta^H_0.
logvar_lad_build_fragment <- function(lad, n_obs, tau_display) {
  tab <- lad$table
  n_pc_r <- length(tab$coef) - 1L
  keys <- vapply(tau_display, paper_tau_key, character(1))
  sets <- lad$sets[keys]
  stopifnot(!any(vapply(sets, is.null, logical(1))))
  labels <- c(
    "$\\theta^{0.5}_0$", sprintf("$\\theta^{0.5}_{%d,R}$", seq_len(n_pc_r))
  )
  rows <- c(interleave(labels, ""), "$R^2$", "$N$")
  cols <- c(
    list(
      logvar_se_point_col(tab$reference, NULL, NULL, NULL, tab$coef, n_obs),
      logvar_se_point_col(tab$point, NULL, NULL, NULL, tab$coef, n_obs)
    ),
    logvar_set_envelope_cols(sets, NULL, names(sets), tab$coef, n_obs)
  )
  build_simple_latex_table(
    rows, cols,
    col_headers = c(
      "Reference", "$\\tau{=}0$", sprintf("$\\tau{=}%.2g$", tau_display)
    ),
    caption = paste(
      "LAD panel: $\\theta^{0.5}$, the conditional-median map over the identified",
      "news sets (attained punctured-domain hulls; closure diagnostics separate)."
    ),
    label = "tab:log_var_eq_panel_lad",
    fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
    rule_after = 2L
  )
}

# Append the marker-wrapped median panel after the incoming (already ordered) panel
# lines. The splice mirrors the host's panel_block shape; kept local so it never
# shadows that global helper. Returns the incoming lines with the median block added.
logvar_lad_append_panel <- function(panels_lines, lad, n_obs,
                                    tau_display, tau_baseline,
                                    grid_cap, fit_budget) {
  fragment <- logvar_lad_build_fragment(lad, n_obs, tau_display)
  notes <- build_lad_panel_notes(lad, tau_baseline, grid_cap, fit_budget)
  c(
    panels_lines,
    logvar_panel_block(
      fragment,
      notes,
      "lad",
      panel_marker = "LOGVAR LAD PANEL"
    )
  )
}

# The median panel notes (dossier section nine): the estimand, the br convention
# and its reason, the exact-versus-guarded domain split, the attained-hull-primary
# and approximate-closure adaptation, side-dependent limits, the dependency and its
# version, the deferred inference, the disclosed search budgets, and the
# interpretive normalization constants. One \item per line so later diffs stay
# block-scoped; guarded so notes are never emitted for a missing panel object.
build_lad_panel_notes <- function(lad, tau_baseline, grid_cap, fit_budget) {
  stopifnot(is.list(lad), !is.null(lad$table))
  caps <- lad$estimator$metadata
  c(
    paste(
      "The median panel fits the conditional median of $\\log \\varepsilon^2$,",
      "$Q_{0.5}(\\log \\varepsilon^2 \\mid PC_R) = R'\\theta^{0.5}$, at each news",
      "vector $b_N$; the intercept $\\theta^{0.5}_0$ is the median normalization,",
      "not the mean-log or log-mean intercept."
    ),
    paste(
      "The inner map uses the Barrodale-Roberts (\\texttt{br}) vertex as the",
      "pre-specified selection rule, so the map stays single-valued and",
      "reproducible where the median is non-unique; \\texttt{fn} is the",
      "nonuniqueness probe, never the map, and coefficients are never averaged."
    ),
    paste(
      "The domain is punctured at the residual-crossing hyperplanes: an exact",
      "residual zero is outside the $\\log$ domain, while a guarded nonzero",
      "residual is in-domain but numerically unresolved. No residual floor is",
      "applied and no row is dropped."
    ),
    paste(
      "The reported cells are single-pass attained inner approximations of the",
      "punctured-domain hulls: every endpoint is a fitted coefficient at a feasible",
      "$b_N$, and the engine's multi-start search only ever extends an endpoint",
      "outward, so the cell is a subset of the true image. An offline refinement",
      "search (up to $10^4$ extra starts per $\\tau$) moved no endpoint on this",
      "sample, so the single-pass cell coincides with the denser search here."
    ),
    paste(
      "The one-sided closure limits are an explicitly approximate diagnostic, kept",
      "separate in \\texttt{log\\_var\\_eq\\_lad\\_closure.csv} and never folded into a",
      "cell; side-dependent finite limits are retained as separate closure rows."
    ),
    sprintf(
      paste(
        "Search resolution: the grid is capped at %d points within a %d-fit",
        "per-slack budget with phase caps; a five-start re-polish verifies each",
        "bounded side, feeding a more extreme candidate back through the engine",
        "and demoting an unreproducible side to unreliable, and the deterministic",
        "\\texttt{fn} schedule fails a nonunique endpoint-relevant tau closed."
      ),
      grid_cap, fit_budget
    ),
    paste(
      "Statuses are operational: bounded, unbounded, unreliable, or unresolved;",
      "no finite probe trace is a certified exclusion, and the crossing witnesses,",
      "path coverage, guard ratio, and $M$-slope metrics are disclosed in the",
      "console block."
    ),
    sprintf(
      paste(
        "\\texttt{quantreg} %s is a scripts-paper analysis dependency (not in",
        "\\texttt{DESCRIPTION}); no standard errors or quantile-regression",
        "confidence intervals are reported (deferred)."
      ),
      lad$quantreg_version
    ),
    sprintf(
      paste(
        "Under conditional normality the median slopes equal the variance slopes",
        "and $\\theta^{0.5}_0 = \\theta^{log}_0 + %.9f = \\theta^{var}_0 -",
        "%.9f$; these constants are interpretive checks only, never imposed."
      ),
      LOGVAR_NORMAL_MEDIAN_MEANLOG_GAP, -LOGVAR_NORMAL_LOG_SQUARE_MEDIAN
    ),
    sprintf(
      paste(
        "The panel order is governed by the benchmark crossing rule at",
        "$\\tau{=}%.2g$; the median panel is appended after that ordered set and",
        "never influences it. %s"
      ),
      tau_baseline,
      if (is.null(caps)) "" else "Cells are inner projection hulls, not point estimates."
    )
  )
}
