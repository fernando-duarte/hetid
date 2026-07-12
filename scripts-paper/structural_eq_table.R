# Publication table for the structural consumption-growth equation estimated
# in set_id_mean_eq.R: one row per coefficient of
# Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1},
# with the OLS benchmark, the closed-form Lewbel point at tau = 0, and the
# exact identified set at the baseline slack. Writes structural_eq.tex, the
# standalone variant, and its compiled PDF to scripts-paper/output/.
# Run via run_all.R after set_id_mean_eq.R.

source("scripts/utils/latex_table_utils.R")
source("scripts/utils/latex_simple_table.R")

# coefficient rows: design block (b_0, b_E) then the news block (b_N); the
# guard pins the row order the labels below assume
coef_tab <- rbind(set_id_mean_eq$beta1_table, set_id_mean_eq$theta_table)
stopifnot(identical(coef_tab$coef, c(
  "(Intercept)", paste0("lag_expected_sdf_pc", seq_len(n_pc)),
  paste0("sdf_news_pc", seq_len(n_pc))
)))

fmt <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
set_cell <- function(lo, hi, status) {
  ifelse(status == "bounded", sprintf("$[%.3f,\\,%.3f]$", lo, hi), status)
}

n_obs <- set_id_mean_eq$sample$n
span <- paste(format(set_id_mean_eq$sample$span), collapse = "--")
r2 <- summary(set_id_mean_eq$ols_fit)$r.squared
tau_base <- set_id_mean_eq$tau_baseline

row_labels <- c(
  "$b_0$",
  sprintf("$b_{%d,E}$", seq_len(n_pc)),
  sprintf("$b_{%d,N}$", seq_len(n_pc)),
  "$R^2$", "$N$"
)
columns <- list(
  c(fmt(coef_tab$ols), sprintf("%.2f", r2), sprintf("%d", n_obs)),
  c(fmt(coef_tab$point), "--", sprintf("%d", n_obs)),
  c(
    set_cell(coef_tab$set_lower, coef_tab$set_upper, coef_tab$status),
    "--", sprintf("%d", n_obs)
  )
)

# data-derived caption: how many news-coefficient sets exclude zero
n_excl <- sum(with(
  set_id_mean_eq$theta_table,
  status == "bounded" & (set_lower > 0 | set_upper < 0)
))
caption <- sprintf(
  paste0(
    "Heteroskedasticity set-identifies the SDF-news coefficients: %d of %d ",
    "components of $b_N$ have identified sets excluding zero at $\\tau{=}%.2g$."
  ),
  n_excl, n_pc, tau_base
)

# notation paragraph, dimensions taken from the estimated system
enum_b <- function(sym) {
  paste0("[", paste(sprintf("b_{%d,%s}", seq_len(n_pc), sym), collapse = ","), "]^{T}")
}
s_max <- max(mats_qtr) %/% step_qtr
notes <- c(
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
  "size $N$. The identification columns treat $PC_{N,t+1}$ as endogenous,",
  sprintf("with %s as the single heteroskedasticity instrument $Z$", z_desc),
  "(Lewbel 2012). At $\\tau{=}0$ the moment conditions",
  "$\\mathrm{Cov}(Z,\\varepsilon_{t+1}\\varepsilon_{2,i,t+1})=0$, with",
  "$\\varepsilon_{2,i,t+1}$ the $i$-th news PC residualized on",
  "$(1,PC_{E,t})$, point-identify $b_{N}$ in closed form; the",
  sprintf("$\\tau{=}%.2g$ column relaxes each condition to an absolute", tau_base),
  "centered correlation of at most $\\tau$ and reports the exact",
  "per-coefficient range of the joint identified set. $b_{0}$ and $b_{E}$ are",
  "recovered as $\\beta_{1}(b_{N})=\\beta_{1}^{R}-(\\beta_{2}^{R})^{T}b_{N}$ and",
  "bounded over the set.",
  sprintf(
    "The joint set remains bounded up to $\\tau^{*}=%.3g$%s.",
    set_id_mean_eq$tau_star,
    if (set_id_mean_eq$tau_star_capped) " (capped at the sweep maximum)" else ""
  ),
  sprintf("$N=%d$, %s.", n_obs, span),
  "Set cells are exact identified-set ranges, not confidence intervals."
)

structural_table <- build_simple_latex_table(
  row_labels, columns,
  col_headers = c("OLS", "$\\tau{=}0$", sprintf("$\\tau{=}%.2g$", tau_base)),
  caption = caption, label = "tab:structural_eq_set_id",
  notes = notes, fontsize = "\\small\\setlength{\\tabcolsep}{4pt}",
  rule_after = c(1L + n_pc, 1L + 2L * n_pc)
)
write_latex_table(structural_table, out_dir, "structural_eq")

# compile the standalone variant so a LaTeX regression fails the pipeline
compile_latex_pdf(file.path(out_dir, "structural_eq_standalone.tex"))

cat(
  sprintf("structural equation table: %d of %d b_N sets exclude zero", n_excl, n_pc),
  sprintf("at tau = %.2g\n", tau_base)
)

rm(
  coef_tab, fmt, set_cell, n_obs, span, r2, tau_base, row_labels, columns,
  n_excl, caption, enum_b, s_max, notes, structural_table
)
