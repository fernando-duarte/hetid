# Publication table for the structural consumption-growth equation estimated
# in estimate_identified_set.R: one row per coefficient of
# Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1},
# with the OLS benchmark (Newey-West t statistics), the closed-form Lewbel
# point at tau = 0, and the exact identified set at each tau_display slack.
# Writes both structural-equation table variants and their standalone PDFs to
# the typed table directory after mean-set estimation.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("mean_equation", "tables", "structural_equation_caption.R"))

# coefficient rows: design block (b_0, b_E) then the news block (b_N); the
# guard pins the row order the labels below assume
coef_tab <- rbind(set_id_mean_eq$beta1_table, set_id_mean_eq$theta_table)
stopifnot(identical(coef_tab$coef, c(
  "(Intercept)", paste0("lag_expected_sdf_pc", seq_len(n_pc)),
  paste0("sdf_news_pc", seq_len(n_pc))
)))

fmt <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
# a degenerate interval (exactly equal endpoints, the point-identified case)
# is left blank: the set equals the tau = 0 point at every displayed tau
set_cell <- function(lo, hi, status) {
  ifelse(
    status != "bounded", status,
    ifelse(lo == hi, "", sprintf("$[%.3f,\\,%.3f]$", lo, hi))
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
ols_tstats <- sprintf("(%.2f)", nw$statistic)

# sampling uncertainty from the endpoint bootstrap (run_bootstrap.R):
# robust nominal 90% intervals under the tau = 0 points in both variants,
# Stoye-calibrated nominal intervals under the set cells in the inference
# variant only; blank cells stay blank
stopifnot(identical(set_id_boot$point_ci$coef, coef_tab$coef))
interval_cell <- function(lo, hi, blank) {
  ifelse(
    blank | !is.finite(lo) | !is.finite(hi), "",
    sprintf("$(%.3f,\\,%.3f)$", lo, hi)
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
  status == "bounded" & (set_lower > 0 | set_upper < 0)
))
caption <- sprintf(
  paste0(
    "Heteroskedasticity set-identifies the SDF-news coefficients: %d of %d ",
    "components of $b_N$ have identified sets excluding zero at $\\tau{=}%.2g$."
  ),
  n_excl, n_pc, tau_base
)

# per-tau exact cells, interval frames, and integrity guards, computed once
# and shared by both variants
set_data <- lapply(names(set_id_mean_eq$set_tables), function(nm) {
  st <- set_id_mean_eq$set_tables[[nm]]
  tab <- rbind(st$beta1, st$theta)
  stopifnot(identical(tab$coef, coef_tab$coef))
  inf <- set_id_boot$inference[[nm]]
  stopifnot(identical(inf$coef, coef_tab$coef))
  list(cells = set_cell(tab$set_lower, tab$set_upper, tab$status), inf = inf)
})

# both variants share every cell except the interval rows under the set
# cells; the conservative variant keeps the manuscript's include filename,
# and both carry the same label -- the manuscript includes exactly one of
# the two files, and a shared label keeps every \ref valid across the swap
variants <- list(
  list(stub = "structural_eq", with_ci = FALSE),
  list(stub = "structural_eq_inference", with_ci = TRUE)
)
for (v in variants) {
  set_columns <- lapply(set_data, function(cell_dat) {
    sub <- if (v$with_ci) {
      interval_cell(
        cell_dat$inf$ci_lower, cell_dat$inf$ci_upper, cell_dat$cells == ""
      )
    } else {
      rep("", length(cell_dat$cells))
    }
    c(interleave(cell_dat$cells, sub), "--", sprintf("%d", n_obs))
  })
  columns <- c(
    list(
      c(
        interleave(ols_cells, ols_tstats),
        sprintf("%.2f", r2), sprintf("%d", n_obs)
      ),
      c(
        interleave(fmt(coef_tab$point), point_ci_cells),
        "--", sprintf("%d", n_obs)
      )
    ),
    unname(set_columns)
  )
  structural_table <- build_simple_latex_table(
    row_labels, columns,
    col_headers = c(
      "OLS", "$\\tau{=}0$",
      sprintf("$\\tau{=}%.2g$", set_id_mean_eq$tau_display)
    ),
    caption = caption, label = "tab:structural_eq_set_id",
    notes = build_structural_notes(with_ci = v$with_ci),
    fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
    rule_after = c(2L * (1L + n_pc), 2L * (1L + 2L * n_pc))
  )
  table_id <- artifact_id(paste0(v$stub, ".tex"))
  standalone_id <- artifact_id(paste0(v$stub, "_standalone.tex"))
  write_latex_table(
    structural_table, artifact_dir(table_id),
    tools::file_path_sans_ext(artifact_basename(table_id))
  )
  compile_latex_pdf(artifact_path(standalone_id))
}

cat(
  sprintf("structural equation table: %d of %d b_N sets exclude zero", n_excl, n_pc),
  sprintf("at tau = %.2g\n", tau_base)
)

rm(
  coef_tab, fmt, set_cell, n_obs, r2, tau_base, row_labels, columns,
  set_columns, nw, ols_cells, ols_tstats,
  coef_labels, n_excl, caption, build_structural_notes, structural_table,
  interval_cell, point_ci_cells, set_data, variants, v, table_id, standalone_id
)
