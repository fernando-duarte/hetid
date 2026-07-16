# Compute the heteroskedasticity battery and Lewbel relevance diagnostics.

source(paper_path("support", "diagnostics", "heteroskedasticity_tests.R"))
source(paper_path("support", "diagnostics", "identification_diagnostics.R"))
source(paper_path("support", "latex", "table_pipeline.R"))
source(paper_path("support", "latex", "simple_table.R"))

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
  stars <- if (x < 0.01) {
    "***"
  } else if (x < 0.05) {
    "**"
  } else if (x < 0.10) {
    "*"
  } else {
    ""
  }
  paste0("{", fmt(x), if (nzchar(stars)) paste0("$^{", stars, "}$"), "}")
}

# Use one diagnostics regime and deflator choice for every news component.
suite_cfg <- select_diagnostics_suite(y2, z_mat)

run_battery <- function(y2_i) {
  fit <- stats::lm(y2_i ~ z, data = data.frame(y2_i = y2_i, z = z))
  mean_resid <- stats::residuals(fit)
  suite <- tryCatch(
    perform_all_hetero_tests(
      fit,
      "news_pc",
      tests = suite_cfg$suite_tests,
      gq_deflator = suite_cfg$gq_deflator,
      gq_alternative = suite_cfg$gq_alternative
    ),
    error = function(e) NULL
  )
  suite_pvals <- if (is.null(suite)) {
    stats::setNames(
      rep(NA_real_, length(suite_cfg$suite_tests)),
      suite_cfg$suite_tests
    )
  } else {
    cols <- grep("_pval$", names(suite), value = TRUE)
    stats::setNames(as.numeric(suite[1, cols]), sub("_pval$", "", cols))
  }
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

sig <- 0.05
reject <- vapply(pvals, function(pv) {
  isTRUE(pv[["BP"]] < sig) ||
    isTRUE(pv[["GQ"]] < sig) ||
    isTRUE(pv[["ARCH"]] < sig)
}, logical(1))
n_pc_tested <- ncol(y2)
caption <- if (any(reject)) {
  sprintf(
    paste0(
      "The instrument drives significant conditional ",
      "heteroskedasticity in %d of %d SDF-news PCs (Lewbel relevance)."
    ),
    sum(reject),
    n_pc_tested
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
