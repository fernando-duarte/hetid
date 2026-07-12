# Heteroskedasticity (Lewbel relevance) tests of the SDF-news PCs against the
# instrument Z (z_col in run_all.R), run on the news PCs directly -- they are
# orthogonal to the lagged expected-SDF design in the population, so no
# residualization step -- with the stage-08 battery (regime-selected skedastic
# suite plus Glejser, Breusch-Pagan LM, and ARCH(1)), one table column per
# news PC, plus the Cov(Z, Y2^2) relevance diagnostics. Writes
# hetero_tests.tex, the standalone variant, and its compiled PDF to
# scripts-paper/output/. Run via run_all.R after set_id_mean_eq.R.

source("scripts/utils/hetero_test_utils.R")
source("scripts/utils/hetero_lm_tests.R")
source("scripts/utils/latex_table_utils.R")
source("scripts/utils/latex_simple_table.R")

w1 <- set_id_mean_eq$w1
y1 <- set_id_mean_eq$y1
y2 <- set_id_mean_eq$y2
z <- set_id_mean_eq$z
z_mat <- matrix(z, ncol = 1, dimnames = list(NULL, "z"))

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
# p-value cell with rejection stars, pre-braced so the siunitx S column
# treats every test cell as text and renders them uniformly
pcell <- function(x) {
  if (!is.finite(x)) {
    return("--")
  }
  stars <- if (x < 0.01) "***" else if (x < 0.05) "**" else if (x < 0.10) "*" else ""
  paste0("{", fmt(x), if (nzchar(stars)) paste0("$^{", stars, "}$"), "}")
}

# one regime/suite/deflator choice shared by every component (Anscombe only
# when the Y2-on-Z refit carries real fitted variation; GQ two-sided in the
# Z order, per the stage-02 investigation)
suite_cfg <- select_diagnostics_suite(y2, z_mat)

# battery for one news PC: OLS mean regression of Y2 on Z, then the suite,
# Glejser, BP LM, and ARCH(1) on its residuals, each wrapped so a failure
# renders "--" instead of aborting the pipeline
run_battery <- function(y2_i) {
  fit <- stats::lm(y2_i ~ z, data = data.frame(y2_i = y2_i, z = z))
  mean_resid <- stats::residuals(fit)
  suite <- tryCatch(
    perform_all_hetero_tests(fit, "news_pc",
      tests = suite_cfg$suite_tests,
      gq_deflator = suite_cfg$gq_deflator,
      gq_alternative = suite_cfg$gq_alternative
    ),
    error = function(e) NULL
  )
  suite_pvals <- if (is.null(suite)) {
    stats::setNames(rep(NA_real_, length(suite_cfg$suite_tests)), suite_cfg$suite_tests)
  } else {
    cols <- grep("_pval$", names(suite), value = TRUE)
    stats::setNames(as.numeric(suite[1, cols]), sub("_pval$", "", cols))
  }
  c(
    suite_pvals,
    Glejser = tryCatch(skedastic::glejser(fit)$p.value, error = function(e) NA_real_),
    BPLM = tryCatch(bp_lm_test(mean_resid, z_mat)$p_value, error = function(e) NA_real_),
    ARCH = tryCatch(arch1_test(mean_resid)$p_value, error = function(e) NA_real_)
  )
}
pvals <- apply(y2, 2, run_battery, simplify = FALSE)

test_labels <- c(
  White = "White ($p$)", BP = "Breusch--Pagan ($p$)",
  GQ = "Goldfeld--Quandt ($p$)", Harvey = "Harvey ($p$)",
  Anscombe = "Anscombe ($p$)", Glejser = "Glejser ($p$)",
  BPLM = "Breusch--Pagan LM ($p$)", ARCH = "ARCH(1) ($p$)"
)
test_names <- c(suite_cfg$suite_tests, "Glejser", "BPLM", "ARCH")

# relevance diagnostics per component; Cov is the centered 1/T moment
column_cells <- function(k) {
  y2_i <- y2[, k]
  pv <- pvals[[k]]
  mean_t <- summary(stats::lm(y2_i ~ z))$coefficients[2, 3]
  c(
    vapply(test_names, function(nm) pcell(pv[[nm]]), character(1)),
    fmt(mean(z * y2_i^2) - mean(z) * mean(y2_i^2)),
    fmt(stats::cor(z, y2_i^2)),
    fmt(mean_t),
    fmt(stats::cor(w1, y2_i)),
    fmt(stats::cov(w1, y2_i) / stats::var(y2_i) * stats::sd(y2_i) / stats::sd(y1)),
    fmt(stats::cov(w1, y2_i) / stats::var(y2_i) * stats::sd(y2_i))
  )
}
cells <- do.call(cbind, lapply(seq_len(ncol(y2)), column_cells))

# joint relevance (rk_rank_test, hetero_lm_tests.R): M_Z-hat = (1/T) sum_t
# (Z_t - Zbar) Y2_t Y2_t' estimates E[(Z - EZ) Var_t(Y2)] when the news have
# mean zero given Z (raw Y2, so the level effect of Z also enters); its
# diagonal is the Cov(Z, Y2^2) row. det, condition number, and smallest
# singular value gauge whether Z moves the news covariance in every
# direction, the rk p-value tests the rank-deficient null; matrix-level
# scalars, shown in one column
rk <- rk_rank_test(y2, z)
fmt_sci <- function(x) sprintf("{\\num{%s}}", formatC(x, format = "g", digits = 3))
joint_cells <- c(
  vapply(c(rk$det, rk$kappa, rk$sv_min), fmt_sci, character(1)),
  pcell(rk$p)
)
cells <- rbind(
  cells,
  cbind(joint_cells, matrix("", length(joint_cells), ncol(y2) - 1L))
)

row_labels <- c(
  unname(test_labels[test_names]),
  "$\\mathrm{Cov}(Z,Y_2^2)$", "$\\mathrm{corr}(Z,Y_2^2)$",
  "$t$-stat of $Y_2$-on-$Z$", "$\\mathrm{corr}(W_1,Y_2)$",
  "$[\\mathrm{Cov}(W_1,Y_2)/\\mathrm{Var}(Y_2)]\\cdot\\mathrm{sd}(Y_2)/\\mathrm{sd}(\\Delta c)$",
  "$[\\mathrm{Cov}(W_1,Y_2)/\\mathrm{Var}(Y_2)]\\cdot\\mathrm{sd}(Y_2)$",
  "$\\det\\widehat{M}_Z$", "$\\kappa(\\widehat{M}_Z)$",
  "$\\sigma_{\\min}(\\widehat{M}_Z)$",
  "Kleibergen--Paap $\\mathrm{rk}$ ($p$)"
)

# caption computed from the results, using the stage-08 rejection rule
# (BP, GQ, or ARCH below the significance level)
sig <- 0.05
reject <- vapply(pvals, function(pv) {
  isTRUE(pv[["BP"]] < sig) || isTRUE(pv[["GQ"]] < sig) || isTRUE(pv[["ARCH"]] < sig)
}, logical(1))
n_pc_tested <- ncol(y2)
caption <- if (any(reject)) {
  sprintf(
    paste0(
      "The instrument drives significant conditional ",
      "heteroskedasticity in %d of %d SDF-news PCs (Lewbel relevance)."
    ),
    sum(reject), n_pc_tested
  )
} else {
  sprintf(
    paste0(
      "The %d SDF-news PCs show no significant conditional ",
      "heteroskedasticity against the instrument (weak Lewbel relevance)."
    ),
    n_pc_tested
  )
}

n_obs <- set_id_mean_eq$sample$n
span <- paste(format(set_id_mean_eq$sample$span), collapse = "--")
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
  "$[\\mathrm{Cov}(W_1,Y_2)/\\mathrm{Var}(Y_2)]\\cdot\\mathrm{sd}(Y_2)/\\mathrm{sd}(\\Delta c)$",
  "rescales that co-movement into standard-deviation units, the move in",
  "consumption growth per one-sd move in the news PC as a fraction of one sd of",
  "raw consumption growth $\\Delta c$; it differs from $\\mathrm{corr}(W_1,Y_2)$",
  "only by the factor $\\mathrm{sd}(W_1)/\\mathrm{sd}(\\Delta c)$. Without the",
  "$\\mathrm{sd}(\\Delta c)$ division the same quantity is in the native units of",
  "$\\Delta c$: percentage points of quarterly consumption growth per one-sd move",
  "in the news PC.",
  sprintf("$N=%d$, %s.", n_obs, span),
  "$^{*}$: $p<0.10$; $^{**}$: $p<0.05$; $^{***}$: $p<0.01$."
)

# split the rows into panels: ARCH probes heteroskedasticity in general
# (own-lag volatility clustering), the rest of the battery probes
# heteroskedasticity driven by Z, and the remaining rows are diagnostics
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
  caption = caption, label = "tab:sdf_news_hetero_tests",
  notes = notes, col_group_label = "SDF-news PC", table_format = "-2.3"
)
write_latex_table(hetero_table, out_dir, "hetero_tests")

# compile the standalone variant so a LaTeX regression fails the pipeline
compile_latex_pdf(file.path(out_dir, "hetero_tests_standalone.tex"))

cat(
  sprintf("hetero tests (Z = %s): regime", z_col),
  suite_cfg$regime, "suite,", n_obs, "obs\n",
  sprintf(
    "KP rk underidentification: stat = %.3g, p = %.3g (NW lag %d, sv sep %.2g)\n",
    rk$stat, rk$p, rk$lag, rk$sep
  )
)
print(do.call(cbind, pvals), digits = 3)

rm(
  w1, y1, y2, z, z_mat, fmt, pcell, suite_cfg, run_battery, pvals, test_labels,
  test_names, column_cells, cells, rk, fmt_sci, joint_cells, row_labels, sig,
  reject, n_pc_tested,
  caption, n_obs, span, notes, panel_rows, arch_row, panels, hetero_table
)
