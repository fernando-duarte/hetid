# Compute the heteroskedasticity battery and Lewbel relevance diagnostics.

paper_source_once(paper_path("support", "diagnostics", "heteroskedasticity_tests.R"))
paper_source_once(paper_path("support", "diagnostics", "identification_diagnostics.R"))
paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))

w1 <- set_id_mean_eq$w1
y1 <- set_id_mean_eq$y1
y2 <- set_id_mean_eq$y2
z <- set_id_mean_eq$z
z_mat <- matrix(z, ncol = 1, dimnames = list(NULL, "z"))

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

pcell <- function(x) {
  if (!is.finite(x)) {
    return("--")
  }
  stars <- sig_stars(x)
  paste0("{", fmt(x), if (nzchar(stars)) paste0("$", stars, "$"), "}")
}

# Use one diagnostics regime and deflator choice for every news component.
suite_cfg <- select_diagnostics_suite(y2, z_mat)

run_battery <- function(y2_i) {
  fit <- stats::lm(y2_i ~ z, data = data.frame(y2_i = y2_i, z = z))
  mean_resid <- stats::residuals(fit)
  # select_diagnostics_suite already pinned a suite and deflator that apply to
  # this design, so a throw here is a defect, not a verdict. It must not be
  # swallowed: an NA p-value reads as "not significant" at the reject test
  # below, so a broken battery would caption the table "weak Lewbel relevance".
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

pvals <- apply(y2, 2, run_battery, simplify = FALSE)
test_labels <- c(
  White = "White ($p$)",
  BP = "Breusch--Pagan ($p$)",
  GQ = "Goldfeld--Quandt ($p$)",
  Harvey = "Harvey ($p$)",
  Anscombe = "Anscombe ($p$)",
  Glejser = "Glejser ($p$)",
  BPLM = "Breusch--Pagan LM ($p$)",
  ARCH = "ARCH(1) ($p$)"
)
test_names <- c(suite_cfg$suite_tests, "Glejser", "BPLM", "ARCH")

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
    fmt(
      stats::cov(w1, y2_i) / stats::var(y2_i) *
        stats::sd(y2_i) / stats::sd(y1)
    ),
    fmt(stats::cov(w1, y2_i) / stats::var(y2_i) * stats::sd(y2_i))
  )
}

cells <- do.call(cbind, lapply(seq_len(ncol(y2)), column_cells))
rk <- rk_rank_test(y2, z)
fmt_sci <- function(x) {
  sprintf("{\\num{%s}}", formatC(x, format = "g", digits = 3))
}
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
  "$\\mathrm{Cov}(Z,Y_2^2)$",
  "$\\mathrm{corr}(Z,Y_2^2)$",
  "$t$-stat of $Y_2$-on-$Z$",
  "$\\mathrm{corr}(W_1,Y_2)$",
  paste0(
    "$[\\mathrm{Cov}(W_1,Y_2)/\\mathrm{Var}(Y_2)]\\cdot",
    "\\mathrm{sd}(Y_2)/\\mathrm{sd}(\\Delta c)$"
  ),
  paste0(
    "$[\\mathrm{Cov}(W_1,Y_2)/\\mathrm{Var}(Y_2)]\\cdot",
    "\\mathrm{sd}(Y_2)$"
  ),
  "$\\det\\widehat{M}_Z$",
  "$\\kappa(\\widehat{M}_Z)$",
  "$\\sigma_{\\min}(\\widehat{M}_Z)$",
  "Kleibergen--Paap $\\mathrm{rk}$ ($p$)"
)

# A PC "rejects" only on a p-value that is a real number below the named
# rejection level. An NA/NaN
# means the test did not run (a caught supplementary failure, or a degenerate
# non-throwing NaN), which is neither rejection nor its opposite -- isTRUE()
# used to fold it into "did not reject" and caption the inverse finding. Keep
# only the finite p-values per PC, so a PC counts as tested only if at least
# one of the caption-driving tests produced a verdict.
caption_tests <- PAPER_HETEROSKEDASTICITY_CONTROL$caption_tests
rejection_alpha <- paper_significance_level(
  PAPER_HETEROSKEDASTICITY_CONTROL$rejection_level
)
caption_p_values <- function(pv) {
  unname(unlist(pv[caption_tests], use.names = FALSE))
}
reject <- vapply(pvals, function(pv) {
  any(Filter(is.finite, caption_p_values(pv)) < rejection_alpha)
}, logical(1))
n_pc_tested <- ncol(y2)
caption <- local({
  tested <- vapply(pvals, function(pv) {
    length(Filter(is.finite, caption_p_values(pv))) > 0L
  }, logical(1))
  n_tested <- sum(tested)
  n_reject <- sum(reject)
  untested <- n_pc_tested - n_tested
  note <- if (untested > 0L) {
    sprintf(" The battery did not run on %d of %d PCs.", untested, n_pc_tested)
  } else {
    ""
  }
  if (n_tested == 0L) {
    sprintf(
      paste0(
        "The conditional-heteroskedasticity battery did not run on any of ",
        "the %d SDF-news PCs, so Lewbel relevance is undetermined."
      ),
      n_pc_tested
    )
  } else if (n_reject > 0L) {
    sprintf(
      paste0(
        "The instrument drives significant conditional heteroskedasticity ",
        "in %d of %d SDF-news PCs (Lewbel relevance).%s"
      ),
      n_reject, n_tested, note
    )
  } else {
    sprintf(
      paste0(
        "The %d SDF-news PCs show no significant conditional heteroskedasticity ",
        "against the instrument (weak Lewbel relevance).%s"
      ),
      n_tested, note
    )
  }
})
n_obs <- set_id_mean_eq$sample$n
span <- paste(format(set_id_mean_eq$sample$span), collapse = "--")
