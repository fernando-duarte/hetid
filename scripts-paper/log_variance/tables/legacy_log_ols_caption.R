# Caption-notes builder retained for the legacy mean-log OLS robustness table:
# notation, two-step construction, set mapping, and honesty clauses for
# residual sign crossings. The primary log_var_eq table now uses PPML notes;
# this builder reads the run_pipeline.R constants and legacy log_var_eq results at
# call time if a separate mean-log table needs the full notes.

paper_source_once(paper_path("support", "reporting", "inference.R"))

build_logvar_notes <- function() {
  n_obs <- log_var_eq$sample$n
  span <- paste(format(log_var_eq$sample$span), collapse = "--")
  c(
    "The estimated equation is",
    "$\\log\\varepsilon_{t+1}^{2}=\\theta_{0}+PC_{R,t}^{T}\\theta_{R}+\\xi_{t+1}$",
    "with $\\varepsilon_{t+1}$ the structural residual of the",
    "consumption-growth equation (Table \\ref{tab:structural_eq_set_id}),",
    sprintf("$\\theta_{R}$ a $%d\\times1$ vector of constant parameters, and", n_pc_r),
    "$\\xi_{t+1}$ a mean-zero error term.",
    sprintf("$PC_{R,t}$ is the $%d\\times1$ vector of the first %d principal", n_pc_r, n_pc_r),
    "components of a cross-section of nominal financial asset returns",
    "(stocks, Treasuries, corporate bonds; not principal components of",
    "yields), dated",
    "$t$ and de-meaned over the estimation sample, so $\\theta_{0}$ is the",
    "sample mean of $\\log\\hat{\\varepsilon}_{t+1}^{2}$ at the reported",
    "coefficients. Because the equation is stated on $\\log\\varepsilon^{2}$",
    "with a mean-zero $\\xi$, $\\theta_{0}$ absorbs $2\\log|m_{0}|$ and the",
    "mean of the log squared standardized innovation; only that normalized",
    "intercept is identified.",
    "The identification columns are a two-step construction: given the news",
    "coefficients $b_{N}$, the fitted residual is",
    "$\\hat{\\varepsilon}_{t+1}(b_{N})=W_{1,t+1}-W_{2,t+1}^{T}b_{N}$ (the",
    "design coefficients are",
    "$\\beta_{1}(b_{N})=\\beta_{1}^{R}-(\\beta_{2}^{R})^{T}b_{N}$, so this is",
    "exact), and $(\\hat{\\theta}_{0},\\hat{\\theta}_{R})(b_{N})$ is the OLS",
    "fit of $\\log\\hat{\\varepsilon}_{t+1}^{2}(b_{N})$ on a constant and",
    "$PC_{R,t}$. The OLS column is instead the naive benchmark: the residuals",
    "of the exogenous-news least-squares fit of the mean equation itself,",
    sprintf(
      paste(
        "log-squared and regressed the same way; numbers in parentheses are",
        "$t$ statistics from %s; %s."
      ),
      paper_newey_west_description(PAPER_REPORTING_CONTROL$logvar_logols),
      paper_significance_legend("descending_p")
    ),
    "These $t$ statistics condition on the fitted first-stage residuals (no",
    "generated-regressor correction), matching the column's naive reading.",
    "The $\\tau{=}0$ column evaluates the two-step map at the closed-form",
    "Lewbel point. Each $\\tau{>}0$ column reports the range of the map over",
    "the joint identified set for $b_{N}$ at that slack (not over the",
    "per-coefficient interval product, which is wider than the joint set).",
    "Finite endpoints are certified feasible values of the map, found by a",
    "feasible-grid scan refined by local re-optimization; on a non-convex",
    "set a farther extremum can in principle escape a local search, so",
    "endpoints are sharp inner approximations rather than certified global",
    "extrema.",
    "$\\log\\hat{\\varepsilon}^{2}$ is singular where a residual crosses",
    "zero. Zero-residual crossings of the joint set are detected by two",
    "tests (per-observation linear-functional bounds over the set, and",
    "residual sign flips across the feasible grid); both are exact on a",
    "connected identified set and err only toward flagging a crossing on a",
    "disconnected one, and a no-crossing verdict shares the local-solver",
    "caveat of every reported bound. Each crossing at observation $t$ drives",
    "$\\hat{\\theta}_{j}$ to $-\\infty$ or $+\\infty$ according to the sign",
    "of the $t$-th projection weight, so affected cells show half-infinite",
    "ranges or unbounded. This divergence is a finite-sample feature of the",
    "log benchmark, not of the population map. Cells marked unreliable had a",
    "side that could not be certified.",
    sprintf(
      "$N=%d$, %s: the mean-equation quarters (its $N=%d$) with $PC_{R,t}$",
      n_obs, span, set_id_mean_eq$sample$n
    ),
    "available.",
    "Set cells are identified-set ranges, not confidence intervals."
  )
}
