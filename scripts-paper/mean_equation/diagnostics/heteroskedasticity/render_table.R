# Render the heteroskedasticity diagnostics as a fragment, standalone source,
# and compiled standalone PDF.

paper_source_once(paper_path(
  "mean_equation",
  "diagnostics",
  "heteroskedasticity",
  "compute_tests.R"
))

notes <- c(
  "Lewbel (2012) relevance requires $\\mathrm{Cov}(Z,\\varepsilon_2^2)\\neq0$;",
  "the null of the $Z$-based tests is homoskedasticity of $Y_2$ given $Z$, the",
  "ARCH(1) null is constant variance over time (no volatility clustering).",
  "$Y_{2,i}$ is SDF-news",
  "PC $i$, tested directly: the news PCs are orthogonal to the lagged",
  "expected-SDF design in the population, so no residualization is needed. $Z$ is",
  sprintf("%s, the single heteroskedasticity driver.", z_desc),
  "Every test starts from the OLS mean regression of $Y_2$ on a constant and $Z$",
  "and probes its residuals: Goldfeld--Quandt orders by $Z$, two-sided; the",
  "Breusch--Pagan LM regresses the squared residuals on $Z$; ARCH(1) regresses",
  "them on their own lag. The $t$-stat of $Y_2$-on-$Z$ is the $t$-statistic on",
  "$Z$ in that same mean regression, so it tests whether $Z$ predicts the level",
  "of $Y_2$. A level effect matters because it enters $Y_2^2$ squared, making $Z$",
  "co-move with $Y_2^2$ even if the variance of $Y_2$ around its conditional mean",
  "never changes. The test $p$-values are free of this effect because the tests",
  "use the mean-regression residuals; the $\\mathrm{Cov}(Z,Y_2^2)$ and",
  "$\\mathrm{corr}(Z,Y_2^2)$ rows use raw $Y_2$ and are not.",
  "$\\widehat{M}_Z=T^{-1}\\sum_t(Z_t-\\bar{Z})\\,Y_{2,t}Y_{2,t}'$ estimates",
  "$\\mathrm{E}[(Z_t-\\mathrm{E}Z_t)\\,\\mathrm{Var}_t(Y_2)]$, the effect of $Z$ on",
  "the full conditional covariance matrix of the news PCs; its diagonal is the",
  "$\\mathrm{Cov}(Z,Y_2^2)$ row, its off-diagonals are",
  "$\\mathrm{Cov}(Z,Y_{2,i}Y_{2,j})$. $\\widehat{M}_Z$ and the $\\mathrm{rk}$ test",
  "also use raw $Y_2$, so the same level-effect caveat applies: the",
  "$\\mathrm{Var}_t$ reading is exact only if the news have mean zero given $Z$,",
  "and a full-rank finding can partly reflect $Z$ moving the level of the news.",
  "Joint relevance is directional.",
  "$\\sigma_{\\min}(\\widehat{M}_Z)$ near zero means some linear combination of the",
  "news PCs has volatility that $Z$ barely moves, so identification is weak in",
  "that direction. $\\det\\widehat{M}_Z$ near zero means $Z$ shifts the covariance",
  "only within a lower-dimensional subspace. $\\kappa(\\widehat{M}_Z)$, the largest",
  "singular value over the smallest, measures how uneven relevance is across",
  "directions; a large value warns that the identified set is much wider along",
  "the weakly moved combinations.",
  "The Kleibergen--Paap $\\mathrm{rk}$ row makes $\\sigma_{\\min}$ inferential: it",
  "tests the null that $\\widehat{M}_Z$ estimates a matrix of rank one less than",
  "the number of news PCs (underidentification: the volatility of some",
  "combination $u'Y_2$ does not respond to $Z$ at all) against full rank. With",
  "one rank deficiency the $\\mathrm{rk}$ statistic reduces to a Newey--West",
  "$t$-test of $\\mathrm{Cov}(Z,(\\hat{u}'Y_2)^2)=0$ at the least-moved",
  sprintf("combination $\\hat{u}$ (Bartlett kernel, %d lags), and is", rk$lag),
  "$\\chi^2(1)$ under the null. The reference distribution also needs the",
  "deficient direction to be unique: the second-smallest singular value of",
  sprintf("$\\widehat{M}_Z$ is %.1f times the smallest, and a ratio near one", rk$sep),
  "would make $\\hat{u}$ ill-determined and the $p$-value unreliable.",
  "A small $p$ rejects underidentification, so $Z$",
  "moves the news covariance in every direction and the joint Lewbel rank",
  "condition holds; a large $p$ means rank deficiency cannot be ruled out.",
  "Rejection alone does not imply strong identification: a significant $p$",
  "with tiny $\\sigma_{\\min}$ still gives wide identified sets.",
  "$W_1$ is consumption",
  "growth residualized on the lagged expected-SDF design, so the structural",
  "equation reads $W_1=\\theta'Y_2+\\varepsilon_1$ and $\\mathrm{Cov}(W_1,Y_2)$",
  "mixes the causal effect $\\theta$ with the shock correlation",
  "$\\mathrm{Cov}(\\varepsilon_1,\\varepsilon_2)$ that makes $Y_2$ endogenous. The",
  "$\\mathrm{corr}(W_1,Y_2)$ row reports this total co-movement, which OLS",
  "attributes entirely to $\\theta$; under $\\theta=0$ it equals",
  "$\\mathrm{corr}(\\varepsilon_1,\\varepsilon_2)$.",
  paste0(
    "$[\\mathrm{Cov}(W_1,Y_2)/\\mathrm{Var}(Y_2)]\\cdot",
    "\\mathrm{sd}(Y_2)/\\mathrm{sd}(\\Delta c)$"
  ),
  "rescales that co-movement into standard-deviation units, the move in",
  "consumption growth per one-sd move in the news PC as a fraction of one sd of",
  "raw consumption growth $\\Delta c$; it differs from $\\mathrm{corr}(W_1,Y_2)$",
  "only by the factor $\\mathrm{sd}(W_1)/\\mathrm{sd}(\\Delta c)$. Without the",
  "$\\mathrm{sd}(\\Delta c)$ division the same quantity is in the native units of",
  "$\\Delta c$: percentage points of quarterly consumption growth per one-sd move",
  "in the news PC.",
  sprintf("$N=%d$, %s.", n_obs, span),
  paste0(paper_significance_legend("ascending_colon"), ".")
)

panel_rows <- function(idx) {
  data.frame(label = row_labels[idx], cells[idx, , drop = FALSE])
}
arch_row <- length(test_names)
panels <- list(
  "Heteroskedasticity in general" = panel_rows(arch_row),
  "Heteroskedasticity driven by $Z$" = panel_rows(seq_len(arch_row - 1L)),
  "Diagnostics" = panel_rows((arch_row + 1L):nrow(cells))
)
hetero_table <- build_panel_latex_table(
  panels,
  col_headers = as.character(seq_len(n_pc_tested)),
  caption = caption,
  label = artifact_latex_label("heteroskedasticity_table"),
  notes = notes,
  col_group_label = "SDF-news PC",
  table_format = "-2.3"
)
publish_latex_artifact("heteroskedasticity_table", hetero_table)

cat(
  sprintf("hetero tests (Z = %s): regime", z_col),
  suite_cfg$regime, "suite,", n_obs, "obs\n",
  sprintf(
    "KP rk underidentification: stat = %s, p = %s (NW lag %d, sv sep %s)\n",
    paper_format_general(
      rk$stat,
      PAPER_REPORTING_CONTROL$precision$console_significant
    ),
    paper_format_general(
      rk$p,
      PAPER_REPORTING_CONTROL$precision$console_significant
    ),
    rk$lag,
    paper_format_general(
      rk$sep,
      PAPER_REPORTING_CONTROL$precision$tau_significant
    )
  )
)
print(
  do.call(cbind, pvals),
  digits =
    PAPER_REPORTING_CONTROL$precision$diagnostic_table
)

rm(
  w1, y1, y2, z, z_mat, fmt, pcell, suite_cfg, run_battery, pvals, test_labels,
  test_names, column_cells, cells, rk, joint_cells, row_labels,
  caption_tests, rejection_alpha, caption_p_values, reject, n_pc_tested,
  caption, n_obs, span, notes, panel_rows, arch_row,
  panels, hetero_table
)
