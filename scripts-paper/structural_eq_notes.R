# Caption-notes builder for the structural-equation table: the notation
# paragraph, the estimation description, and the mode-dependent clauses.
# Sourced at the top of structural_eq_table.R; reads the run_all.R constants
# (n_pc, mats_qtr, step_qtr, z_desc, impose_beta2r_null) and the
# set_id_mean_eq result list at call time. with_ci selects the closing
# paragraph for the conservative (FALSE) or inference (TRUE) variant.

build_structural_notes <- function(with_ci = FALSE) {
  n_obs <- set_id_mean_eq$sample$n
  span <- paste(format(set_id_mean_eq$sample$span), collapse = "--")

  # mode-dependent clauses: under the orthogonality null the news block enters
  # unresidualized (beta2R = 0), the design coefficients degenerate to beta1R
  eps2_note <- if (impose_beta2r_null) {
    c(
      "$\\varepsilon_{2,i,t+1}$ the $i$-th news PC itself (the maintained",
      "orthogonality null imposes $\\beta_{2}^{R}=0$, so the news block enters",
      "unresidualized), point-identify $b_{N}$ in closed form; the"
    )
  } else {
    c(
      "$\\varepsilon_{2,i,t+1}$ the $i$-th news PC residualized on",
      "$(1,PC_{E,t})$, point-identify $b_{N}$ in closed form; the"
    )
  }
  recovery_note <- if (impose_beta2r_null) {
    c(
      "recovered as $\\beta_{1}(b_{N})=\\beta_{1}^{R}-(\\beta_{2}^{R})^{T}b_{N}$,",
      "degenerate at $\\beta_{1}^{R}$ under the imposed $\\beta_{2}^{R}=0$."
    )
  } else {
    c(
      "recovered as $\\beta_{1}(b_{N})=\\beta_{1}^{R}-(\\beta_{2}^{R})^{T}b_{N}$ and",
      "bounded over the set."
    )
  }

  # notation paragraph, dimensions taken from the estimated system
  enum_b <- function(sym) {
    paste0("[", paste(sprintf("b_{%d,%s}", seq_len(n_pc), sym), collapse = ","), "]^{T}")
  }
  s_max <- max(mats_qtr) %/% step_qtr
  c(
    "Let $\\mathrm{SDF}_{t+1}$ be the one-period stochastic discount factor,",
    "so that $\\mathbb{E}_{t}[\\mathrm{SDF}_{t+1}R_{i,t+1}]=1$ for all traded",
    "returns $R_{i,t+1}$, and let $\\Delta c_{t+1}$ be log consumption growth",
    "between $t$ and $t+1$. The estimated equation is",
    "$\\Delta c_{t+1}=b_{0}+PC_{E,t}^{T}b_{E}+PC_{N,t+1}^{T}b_{N}+\\varepsilon_{t+1}$",
    "with $\\varepsilon_{t+1}:=m_{0}(\\mathrm{SDF}_{t+1}-\\mathbb{E}_{t}\\mathrm{SDF}_{t+1})$.",
    "$b_{0}$ and $m_{0}$ are constant scalar parameters;",
    sprintf("$b_{E}=%s$ and $b_{N}=%s$ are", enum_b("E"), enum_b("N")),
    sprintf("$%d\\times1$ vectors of constant parameters.", n_pc),
    sprintf("$PC_{E,t}$ is the $%d\\times1$ vector of the first %d", n_pc, n_pc),
    "principal components of $\\mathbb{E}_{t}[\\mathrm{SDF}_{t+1+s}]$ and",
    sprintf("$PC_{N,t+1}$ the $%d\\times1$ vector of the first %d", n_pc, n_pc),
    "principal components of",
    "$(\\mathbb{E}_{t+1}-\\mathbb{E}_{t})\\mathrm{SDF}_{t+1+s}$, both for",
    sprintf(
      "$s=i/%d$ with $i=%d,%d,\\ldots,%d$ maturity months",
      step_qtr, min(mats_qtr), min(mats_qtr) + 1L, max(mats_qtr)
    ),
    sprintf(
      "(monthly-spaced horizons of $%d$ to $%d$ quarters).",
      min(mats_qtr) %/% step_qtr, s_max
    ),
    "$\\mathbb{E}_{t}[\\mathrm{SDF}_{t+1+s}]$ and",
    "$(\\mathbb{E}_{t+1}-\\mathbb{E}_{t})\\mathrm{SDF}_{t+1+s}$ are observed, and",
    "hence $PC_{E,t}$ and $PC_{N,t+1}$ are also observed; $\\Delta c_{t+1}$ is",
    "observed and $\\varepsilon_{t+1}$ is unobserved. Principal components are",
    "unconditionally orthogonal and mean zero by construction, but not",
    "necessarily conditionally orthogonal.",
    "The OLS column treats $PC_{N,t+1}$ as exogenous: a single least-squares",
    "fit of the equation on the estimation sample, with its $R^2$ and sample",
    "size $N$. Numbers in parentheses below the OLS estimates are $t$",
    "statistics based on Newey--West heteroskedasticity- and",
    "autocorrelation-consistent standard errors with 4 lags (Bartlett kernel,",
    "no prewhitening); $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
    "The identification columns treat $PC_{N,t+1}$ as endogenous,",
    sprintf("with %s as the single heteroskedasticity instrument $Z$", z_desc),
    "(Lewbel 2012). At $\\tau{=}0$ the moment conditions",
    "$\\mathrm{Cov}(Z,\\varepsilon_{t+1}\\varepsilon_{2,i,t+1})=0$, with",
    eps2_note,
    sprintf(
      "$\\tau{>}0$ columns ($\\tau\\in\\{%s\\}$) relax each condition to an",
      paste(sprintf("%.2g", set_id_mean_eq$tau_display), collapse = ",\\,")
    ),
    "absolute centered correlation of at most $\\tau$ and report the exact",
    "per-coefficient range of the joint identified set. $b_{0}$ and $b_{E}$ are",
    recovery_note,
    sprintf(
      "The joint set remains bounded up to $\\tau^{*}=%.3g$%s",
      set_id_mean_eq$tau_star,
      if (set_id_mean_eq$tau_star_capped) " (capped at the sweep maximum)" else ""
    ),
    sprintf(
      "(5th--95th moving-block bootstrap percentile range $[%.2g,\\,%.2g]$,",
      set_id_boot$tau_star_band[["p05"]], set_id_boot$tau_star_band[["p95"]]
    ),
    sprintf(
      "$B=%d$, block $=%d$ quarters%s%s), and stays bounded at the baseline",
      set_id_boot$b_reps, set_id_boot$block,
      if (set_id_boot$n_capped > 0) {
        sprintf("; %d draws censored at the sweep cap", set_id_boot$n_capped)
      } else {
        ""
      },
      if (set_id_boot$n_failed > 0) {
        sprintf("; %d failed draws excluded", set_id_boot$n_failed)
      } else {
        ""
      }
    ),
    sprintf(
      "slack in %.0f\\%% of draws.",
      100 * set_id_boot$tau_star_share_bounded
    ),
    sprintf("$N=%d$, %s.", n_obs, span),
    "Set cells are exact identified-set ranges, not confidence intervals;",
    "a blank set cell marks a point-identified coefficient, whose set equals",
    "the $\\tau{=}0$ point at every displayed $\\tau$.",
    "Parentheses beneath the $\\tau{=}0$ estimates are nominal 90\\% intervals:",
    "the closed-form point plus or minus the one-sided 90\\% normal quantile",
    "times a robust bootstrap standard error (median-absolute-deviation scale",
    "of the moving-block point draws); nominal under maintained regular",
    "asymptotics for the point estimator, and omitted when fewer than half",
    "the draws yield a full-rank $\\tau{=}0$ system.",
    if (with_ci) {
      c(
        "Parenthesized intervals beneath the set cells are nominal 90\\%",
        "Stoye (2009) intervals for the true coefficient: the exact set",
        "endpoints padded by robust (median-absolute-deviation) bootstrap",
        "endpoint standard errors, with the critical value calibrated",
        "against the joint normal distribution of the endpoint estimators",
        "at the correlation estimated from the draws (it coincides with the",
        "Imbens--Manski (2004) interpolation at the estimated correlations",
        "while not requiring the Imbens--Manski superefficient-width",
        "assumption).",
        "These are diagnostics under maintained regular endpoint asymptotics,",
        "componentwise rather than a joint confidence region, and",
        "conditional on the estimated SDF panels, their principal",
        "components, and the realized instrument, all constructed once from",
        "the full sample. An interval row is omitted when fewer than half",
        "the draws produce a certified bounded set at that $\\tau$ or the",
        "endpoint scale is degenerate; per-cell draw counts and omission",
        "reasons are in set\\_id\\_inference\\_diagnostics.csv."
      )
    } else {
      c(
        "Confidence statements for the $\\tau{>}0$ sets await a coverage",
        "validation study; nominal interval diagnostics are reported in the",
        "companion inference table and set\\_id\\_inference\\_diagnostics.csv."
      )
    }
  )
}
