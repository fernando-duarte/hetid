# Regime-aware metadata for the stage-02 heteroskedasticity exports: panel
# row columns/labels for the suite the diagnostics stage actually ran, and
# the LaTeX table notes. Moved from output_results.R, their only consumer,
# to keep that script under the size limit. Regime A excludes Anscombe as
# 0/0-degenerate, Regime B includes it; Cook--Weisberg is excluded in both
# regimes; see docs/reviews/hetero-test-investigation-2026-06-10.md.

hetero_suite_meta <- function(parameters, corr_mat) {
  regime <- parameters$regime
  if (is.null(regime)) regime <- "A"
  gq_label <- if (is.null(parameters$gq_deflator)) {
    "Goldfeld--Quandt"
  } else {
    sprintf(
      "Goldfeld--Quandt (%s, %s)",
      parameters$gq_deflator, parameters$gq_alternative
    )
  }
  suite_names <- parameters$suite_tests
  if (is.null(suite_names)) suite_names <- c("White", "BP", "GQ", "Harvey")
  display_label <- c(
    White = "White", BP = "Breusch--Pagan", GQ = gq_label,
    Harvey = "Harvey", Anscombe = "Anscombe", CW = "Cook--Weisberg",
    Glejser = "Glejser"
  )
  corr_labels <- rownames(corr_mat)
  if (is.null(corr_labels)) corr_labels <- paste0("PC", seq_len(nrow(corr_mat)))
  list(
    regime = regime,
    suite_pval_cols = paste0(c(suite_names, "Glejser"), "_pval"),
    suite_labels = unname(display_label[c(suite_names, "Glejser")]),
    # Instrument symbol for labels/notes: historical PC wording in Regime A,
    # generic Z wording under hook-supplied instruments
    inst_symbol = if (regime == "B") "Z" else "\\mathrm{PC}",
    bplm_label = sprintf(
      "BP LM on %s ($nR^2$, $\\chi^2_{%d}$)",
      if (regime == "B") "Z" else "PCs", nrow(corr_mat)
    ),
    corr_labels = corr_labels
  )
}

hetero_table_notes <- function(hetero, meta, n_pcs, n_obs, corr_mat, tbm) {
  c(
    paste0(
      "$W_{2,i}$ is the residual from the maturity-$i$ yield equation ",
      "regressed on the first ", n_pcs, " principal components (T = ",
      n_obs, " quarterly observations)."
    ),
    paste0(
      "Panels A and B report p-values; rejection of homoskedasticity ",
      "supports the identifying assumption that ",
      "$\\mathrm{Var}(\\varepsilon_2 \\mid Z)$ varies with $Z$."
    ),
    paste0(
      "The BP LM test regresses $W_{2,i}^2$ on the ",
      if (meta$regime == "B") "instruments $Z$ " else "principal components ",
      "($nR^2$, $\\chi^2_{", nrow(corr_mat), "}$); ARCH(1) regresses ",
      "$W_{2,i}^2$ on its own lag."
    ),
    if (meta$regime == "B") {
      paste0(
        "Hook-supplied instruments lie outside the span of the first-stage ",
        "regressors (Regime B): Panel A tests the variance of the $W_{2,i}$-",
        "on-$Z$ refit residuals, the BP LM row is the direct test of ",
        "$\\mathrm{Cov}(Z, W_{2,i}^2)$, and Anscombe (fitted-value based) ",
        "is well-defined and included."
      )
    },
    if (is.null(hetero$parameters$gq_deflator)) {
      "The Goldfeld--Quandt test splits the sample in observation (time) order."
    } else {
      paste0(
        "The Goldfeld--Quandt test orders observations by ",
        hetero$parameters$gq_deflator, " (",
        hetero$parameters$gq_alternative, ")."
      )
    },
    if (!is.null(hetero$w2_cross_maturity_abs_cor_range)) {
      sprintf(
        paste0(
          "Cross-maturity absolute correlations of $W_{2,i}$ span ",
          "%.2f--%.2f, so rejections across maturities are one signal ",
          "measured %d ways, not independent confirmations."
        ),
        hetero$w2_cross_maturity_abs_cor_range[1],
        hetero$w2_cross_maturity_abs_cor_range[2],
        nrow(tbm)
      )
    },
    paste0(
      "Panel C reports $|\\mathrm{corr}(", meta$inst_symbol,
      "_j, W_{2,i}^2)|$."
    )
  )
}
