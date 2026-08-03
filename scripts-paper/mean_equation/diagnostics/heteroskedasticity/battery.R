# Heteroskedasticity battery and relevance diagnostics for one conditioning
# matrix. Parameterized so the same table can be produced for Y2 (the SDF-news
# PCs) and for W2 (the reduced-form residuals identification actually draws its
# curvature from). Every dependency is passed explicitly rather than resolved
# from .GlobalEnv, so the two calls cannot leak state into one another.

paper_source_once(paper_path("support", "diagnostics", "heteroskedasticity_tests.R"))
paper_source_once(paper_path("support", "diagnostics", "identification_diagnostics.R"))

# One column's battery. select_diagnostics_suite has already pinned a suite and
# deflator that apply to this design, so a throw here is a defect, not a
# verdict. It must not be swallowed: an NA p-value reads as "not significant" at
# the reject test below, so a broken battery would caption the table "weak
# Lewbel relevance".
hetero_run_battery <- function(col, z, z_mat, suite_cfg) {
  fit <- stats::lm(col ~ z, data = data.frame(col = col, z = z))
  mean_resid <- stats::residuals(fit)
  suite <- perform_all_hetero_tests(
    fit,
    "news_pc",
    tests = suite_cfg$suite_tests,
    gq_deflator = suite_cfg$gq_deflator,
    gq_alternative = suite_cfg$gq_alternative
  )
  cols <- grep("_pval$", names(suite), value = TRUE)
  suite_pvals <- stats::setNames(as.numeric(suite[1, cols]), sub("_pval$", "", cols))
  c(
    suite_pvals,
    Glejser = tryCatch(
      skedastic::glejser(fit)$p.value,
      error = function(e) NA_real_
    ),
    BPLM = tryCatch(
      bp_lm_test(mean_resid, z_mat)$p_value,
      error = function(e) NA_real_
    ),
    ARCH = tryCatch(
      arch1_test(mean_resid)$p_value,
      error = function(e) NA_real_
    )
  )
}

HETERO_TEST_LABELS <- c(
  White = "White ($p$)",
  BP = "Breusch--Pagan ($p$)",
  GQ = "Goldfeld--Quandt ($p$)",
  Harvey = "Harvey ($p$)",
  Anscombe = "Anscombe ($p$)",
  Glejser = "Glejser ($p$)",
  BPLM = "Breusch--Pagan LM ($p$)",
  ARCH = "ARCH(1) ($p$)"
)

# Row labels carry the conditioning symbol so the two tables cannot be confused
# for one another once separated from their captions.
hetero_row_labels <- function(test_names, sym) {
  c(
    unname(HETERO_TEST_LABELS[test_names]),
    sprintf("$\\mathrm{Cov}(Z,%s^2)$", sym),
    sprintf("$\\mathrm{corr}(Z,%s^2)$", sym),
    sprintf("$t$-stat of $%s$-on-$Z$", sym),
    sprintf("$\\mathrm{corr}(W_1,%s)$", sym),
    sprintf(
      "$[\\mathrm{Cov}(W_1,%s)/\\mathrm{Var}(%s)]\\cdot\\mathrm{sd}(%s)/\\mathrm{sd}(\\Delta c)$",
      sym, sym, sym
    ),
    sprintf(
      "$[\\mathrm{Cov}(W_1,%s)/\\mathrm{Var}(%s)]\\cdot\\mathrm{sd}(%s)$",
      sym, sym, sym
    ),
    "$\\det\\widehat{M}_Z$",
    "$\\kappa(\\widehat{M}_Z)$",
    "$\\sigma_{\\min}(\\widehat{M}_Z)$",
    "Kleibergen--Paap $\\mathrm{rk}$ ($p$)"
  )
}

# A column "rejects" only on a p-value that is a real number below the named
# rejection level. An NA/NaN means the test did not run (a caught supplementary
# failure, or a degenerate non-throwing NaN), which is neither rejection nor its
# opposite -- isTRUE() used to fold it into "did not reject" and caption the
# inverse finding. Keep only the finite p-values per column, so a column counts
# as tested only if at least one caption-driving test produced a verdict.
hetero_caption_p_values <- function(pv) {
  caption_tests <- PAPER_HETEROSKEDASTICITY_CONTROL$caption_tests
  unname(unlist(pv[caption_tests], use.names = FALSE))
}

hetero_caption <- function(pvals, n_cols, subject) {
  alpha <- paper_significance_level(
    PAPER_HETEROSKEDASTICITY_CONTROL$rejection_level
  )
  finite_p <- lapply(pvals, function(pv) Filter(is.finite, hetero_caption_p_values(pv)))
  tested <- vapply(finite_p, function(p) length(p) > 0L, logical(1))
  n_reject <- sum(vapply(finite_p, function(p) any(p < alpha), logical(1)))
  n_tested <- sum(tested)
  untested <- n_cols - n_tested
  note <- if (untested > 0L) {
    sprintf(" The battery did not run on %d of %d %s.", untested, n_cols, subject)
  } else {
    ""
  }
  if (n_tested == 0L) {
    sprintf(
      paste0(
        "The conditional-heteroskedasticity battery did not run on any of the ",
        "%d %s, so Lewbel relevance is undetermined."
      ),
      n_cols, subject
    )
  } else if (n_reject > 0L) {
    sprintf(
      paste0(
        "The instrument drives significant conditional heteroskedasticity in ",
        "%d of %d %s (Lewbel relevance).%s"
      ),
      n_reject, n_tested, subject, note
    )
  } else {
    sprintf(
      paste0(
        "The %d %s show no significant conditional heteroskedasticity against ",
        "the instrument (weak Lewbel relevance).%s"
      ),
      n_tested, subject, note
    )
  }
}

# Full panel for one conditioning matrix. Returns everything the renderer needs,
# with the diagnostics suite selected from this matrix's own design rather than
# inherited: select_diagnostics_suite gates on a fitted-sd ratio, so a suite
# pinned for a different matrix is not guaranteed to apply. The chosen regime is
# reported, so a divergence between the two panels is visible rather than silent.
hetero_panel <- function(mat, sym, subject, w1, y1, z, z_mat, fmt, pcell) {
  suite_cfg <- select_diagnostics_suite(mat, z_mat)
  pvals <- apply(mat, 2, hetero_run_battery,
    z = z, z_mat = z_mat, suite_cfg = suite_cfg, simplify = FALSE
  )
  test_names <- c(suite_cfg$suite_tests, "Glejser", "BPLM", "ARCH")
  column_cells <- function(k) {
    col <- mat[, k]
    pv <- pvals[[k]]
    mean_t <- summary(stats::lm(col ~ z))$coefficients[2, 3]
    c(
      vapply(test_names, function(nm) pcell(pv[[nm]]), character(1)),
      fmt(mean(z * col^2) - mean(z) * mean(col^2)),
      fmt(stats::cor(z, col^2)),
      fmt(mean_t),
      fmt(stats::cor(w1, col)),
      fmt(
        stats::cov(w1, col) / stats::var(col) *
          stats::sd(col) / stats::sd(y1)
      ),
      fmt(stats::cov(w1, col) / stats::var(col) * stats::sd(col))
    )
  }
  cells <- do.call(cbind, lapply(seq_len(ncol(mat)), column_cells))
  rk <- rk_rank_test(mat, z)
  joint_cells <- c(
    paper_format_sci(
      c(rk$det, rk$kappa, rk$sv_min),
      digits = PAPER_REPORTING_CONTROL$precision$diagnostic_table,
      format = "g"
    ),
    pcell(rk$p)
  )
  cells <- rbind(
    cells,
    cbind(joint_cells, matrix("", length(joint_cells), ncol(mat) - 1L))
  )
  list(
    cells = cells,
    row_labels = hetero_row_labels(test_names, sym),
    test_names = test_names,
    pvals = pvals,
    rk = rk,
    suite_cfg = suite_cfg,
    n_cols = ncol(mat),
    caption = hetero_caption(pvals, ncol(mat), subject)
  )
}
