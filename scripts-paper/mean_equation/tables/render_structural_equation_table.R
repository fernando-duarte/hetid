# Publication table for the structural consumption-growth equation estimated
# in estimate_identified_set.R: one row per coefficient of
# Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1},
# with the OLS benchmark (Newey-West t statistics), the closed-form Lewbel
# point at tau = 0, and the exact identified set at each tau_display slack.
# Writes the structural-equation inference table and its standalone PDF to
# the typed table directory after mean-set estimation.
paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("support", "reporting", "cells.R"))
paper_source_once(paper_path("mean_equation", "tables", "structural_equation_caption.R"))
# coefficient rows: design block (b_0, b_E) then the news block (b_N); the
# guard pins the row order the labels below assume
coef_tab <- rbind(set_id_mean_eq$beta1_table, set_id_mean_eq$theta_table)
stopifnot(identical(coef_tab$coef, c(
  PAPER_ANALYSIS_CONTRACT$model$intercept_col,
  PAPER_ANALYSIS_CONTRACT$model$lag_expected_pc_cols,
  PAPER_ANALYSIS_CONTRACT$model$news_pc_cols
)))
fmt <- function(x) {
  policy <- PAPER_REPORTING_CONTROL$cells$structural
  paper_format_number(x, policy$digits, policy$numeric_missing)
}
# a degenerate interval (exactly equal endpoints, the point-identified case)
# is left blank: the set equals the tau = 0 point at every displayed tau
set_cell <- function(lo, hi, status) {
  policy <- PAPER_REPORTING_CONTROL$cells$structural
  paper_format_set_interval(
    lo,
    hi,
    status,
    digits = policy$digits,
    status_mode = policy$status_mode,
    na_as_status = policy$na_as_status,
    infinite_bounds = policy$infinite_bounds,
    degenerate_rtol = policy$degenerate_rtol
  )
}
n_obs <- set_id_mean_eq$sample$n
r2 <- summary(set_id_mean_eq$ols_fit)$r.squared
tau_base <- set_id_mean_eq$tau_baseline
# Newey-West t statistics and significance stars for the OLS column.
nw <- paper_newey_west_statistics(
  set_id_mean_eq$ols_fit,
  coef_tab$ols,
  coef_tab$coef,
  PAPER_REPORTING_CONTROL$mean_ols
)
ols_cells <- ifelse(
  nw$stars == "", fmt(coef_tab$ols),
  sprintf("%s$%s$", fmt(coef_tab$ols), nw$stars)
)
ols_tstats <- sprintf(
  "(%s)",
  paper_format_number(
    nw$statistic,
    PAPER_REPORTING_CONTROL$cells$statistic_digits,
    "na"
  )
)
# sampling uncertainty from the endpoint bootstrap (run_bootstrap.R):
# robust nominal 90% intervals under the tau = 0 points, and Stoye-calibrated
# nominal intervals under the set cells; blank cells stay blank
stopifnot(identical(set_id_boot$point_ci$coef, coef_tab$coef))
interval_cell <- function(lo, hi, blank) {
  policy <- PAPER_REPORTING_CONTROL$cells$structural
  paper_format_confidence_interval(
    lo,
    hi,
    digits = policy$digits,
    blank = blank,
    brackets = policy$confidence_brackets
  )
}
point_ci_cells <- interval_cell(
  set_id_boot$point_ci$lower, set_id_boot$point_ci$upper,
  !is.finite(coef_tab$point)
)
coef_labels <- c(
  "$b_0$",
  sprintf("$b_{%d,E}$", seq_len(n_pc)),
  sprintf("$b_{%d,N}$", seq_len(n_pc))
)
row_labels <- c(interleave(coef_labels, ""), "$R^2$", "$N$")
# data-derived caption: how many news-coefficient sets exclude zero
n_excl <- sum(with(
  set_id_mean_eq$theta_table,
  status == PAPER_ENDPOINT_STATUS[["bounded"]] & (set_lower > 0 | set_upper < 0)
))
caption <- sprintf(
  paste0(
    "Heteroskedasticity set-identifies the SDF-news coefficients: %d of %d ",
    "components of $b_N$ have identified sets excluding zero at $\\tau{=}%s$."
  ),
  n_excl,
  n_pc,
  paper_format_tau(tau_base)
)
# per-tau exact cells, interval frames, and integrity guards, computed once
# for the inference table and its standalone
set_data <- lapply(names(set_id_mean_eq$set_tables), function(nm) {
  st <- set_id_mean_eq$set_tables[[nm]]
  tab <- rbind(st$beta1, st$theta)
  stopifnot(identical(tab$coef, coef_tab$coef))
  inf <- set_id_boot$inference[[nm]]
  stopifnot(identical(inf$coef, coef_tab$coef))
  list(cells = set_cell(tab$set_lower, tab$set_upper, tab$status), inf = inf)
})
artifact_id <- artifact_variant_id("structural_equation", "inference")
set_columns <- lapply(set_data, function(cell_dat) {
  sub <- interval_cell(
    cell_dat$inf$ci_lower, cell_dat$inf$ci_upper, cell_dat$cells == ""
  )
  c(interleave(cell_dat$cells, sub), PAPER_NA_TOKEN, sprintf("%d", n_obs))
})
columns <- c(
  list(
    c(
      interleave(ols_cells, ols_tstats),
      paper_format_number(
        r2,
        PAPER_REPORTING_CONTROL$cells$statistic_digits,
        "na"
      ),
      sprintf("%d", n_obs)
    ),
    c(
      interleave(fmt(coef_tab$point), point_ci_cells),
      PAPER_NA_TOKEN, sprintf("%d", n_obs)
    )
  ),
  unname(set_columns)
)
structural_table <- build_simple_latex_table(
  row_labels, columns,
  col_headers = paper_tau_col_headers(set_id_mean_eq$tau_display),
  caption = caption,
  label = artifact_latex_label(artifact_id),
  notes = build_structural_notes(),
  fontsize = PAPER_TABLE_STYLE$coefficient$fontsize,
  rule_after = c(2L * (1L + n_pc), 2L * (1L + 2L * n_pc))
)
publish_latex_artifact(artifact_id, structural_table)
cat(
  sprintf("structural equation table: %d of %d b_N sets exclude zero", n_excl, n_pc),
  sprintf(
    "at tau = %s\n",
    paper_format_tau(tau_base)
  )
)
rm(
  coef_tab, fmt, set_cell, n_obs, r2, tau_base, row_labels, columns,
  set_columns, nw, ols_cells, ols_tstats,
  coef_labels, n_excl, caption, build_structural_notes, structural_table,
  interval_cell, point_ci_cells, set_data, artifact_id
)
