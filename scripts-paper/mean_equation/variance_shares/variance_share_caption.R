# Caption-notes builder for the variance-share table: the share definition,
# the column semantics, and the computation of the block-row ranges.
# Sourced at the top of render_variance_share_table.R; reads the run_pipeline.R constants
# (impose_beta2r_null) and the set_id_mean_eq result list at call time.

build_var_share_notes <- function(sd_c) {
  n_obs <- set_id_mean_eq$sample$n
  span <- paste(format(set_id_mean_eq$sample$span), collapse = "--")
  grid_points <-
    PAPER_ANALYSIS_CONTRACT$variance_share$grid_points_per_axis

  # mode-dependent clause: under the orthogonality null b_E is point
  # identified, so its set cells degenerate to blanks
  e_block_note <- if (impose_beta2r_null) {
    c(
      "$b_{E}$ is point identified under the maintained",
      "$\\beta_{2}^{R}=0$ null, so its set cells are blank: the share",
      "equals the $\\tau{=}0$ value at every displayed $\\tau$."
    )
  } else {
    c(
      "$b_{E}$ varies over the set through",
      "$\\beta_{1}(b_{N})=\\beta_{1}^{R}-(\\beta_{2}^{R})^{T}b_{N}$, and its",
      "share ranges are computed over the same joint set."
    )
  }

  c(
    "Shares, in percent, of the variance of quarterly log consumption",
    "growth $\\Delta c_{t+1}$ attributable to the expected-SDF block",
    "$PC_{E,t}^{T}b_{E}$ and the SDF-news block $PC_{N,t+1}^{T}b_{N}$ of",
    "the structural equation",
    "$\\Delta c_{t+1}=b_{0}+PC_{E,t}^{T}b_{E}+PC_{N,t+1}^{T}b_{N}+\\varepsilon_{t+1}$",
    "of Table~\\ref{tab:structural_eq_set_id}, whose notes define the",
    "notation, data, and estimation. Block rows report",
    "$100\\,\\widehat{\\mathrm{Var}}(PC^{T}b)/\\widehat{\\mathrm{Var}}(\\Delta c_{t+1})$,",
    "with $\\widehat{\\mathrm{Var}}$ the centered $1/T$ sample moment on the",
    "estimation sample; component rows report",
    "$100\\,b_{k}^{2}\\widehat{\\mathrm{Var}}(PC_{k})/\\widehat{\\mathrm{Var}}(\\Delta c_{t+1})$",
    "for one component alone. Principal components are unconditionally",
    "orthogonal in sample, so at any fixed coefficient vector the component",
    "rows add up to their block row; in the OLS and $\\tau{=}0$ columns the",
    "displayed component cells are rounded by largest remainder so they also",
    "add up exactly to the displayed block cell, each within one $0.01$ step",
    "of its unconstrained rounding.",
    "The OLS column evaluates the shares at the least-squares coefficients",
    "(news PCs treated as exogenous) and the $\\tau{=}0$ column at the",
    "closed-form Lewbel point. The $\\tau{>}0$ columns report the range of",
    "each share as the coefficients vary over the joint identified set at",
    "slack $\\tau$: component rows map the exact per-coefficient ranges of",
    "Table~\\ref{tab:structural_eq_set_id} through the square (the minimum",
    "is zero when the range covers zero), and block rows minimize and",
    "maximize the quadratic share over the joint set (a feasibility grid of",
    sprintf(
      "%d points per axis over the per-coefficient box, checked against the",
      grid_points
    ),
    "joint quadratic constraints, seeds a multistart SLSQP polish along the",
    "constraint boundary, with the grid extremes kept as a feasible",
    "fallback envelope), accurate well past the displayed precision.",
    e_block_note,
    "Shares are relative to the total variance of consumption growth, not",
    "an $R^{2}$ decomposition: away from the least-squares fit the implied",
    "residual $\\varepsilon_{t+1}$ co-moves with the regressor blocks, so",
    sprintf(
      "shares can exceed 100 as $\\tau$ approaches $\\tau^{*}=%.3g$.",
      set_id_mean_eq$tau_star
    ),
    "Set cells are exact identified-set ranges, not confidence intervals.",
    sprintf(
      "For scale, $\\widehat{\\mathrm{sd}}(\\Delta c_{t+1})=%.2f$ percent",
      sd_c
    ),
    sprintf("per quarter. $N=%d$, %s.", n_obs, span)
  )
}
