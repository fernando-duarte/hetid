# Combined inference table stacking the mean equation (Panel A) over the PPML
# log-variance equation (Panel B) under one shared OLS / tau column header:
#   Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1}   (Panel A)
#   E[eps_{t+1}^2 | PC_{R,t}] = exp(theta_0 + PC_{R,t}' theta_R)         (Panel B)
# Panel A reuses structural_equation_table_parts (mean-set estimate + endpoint
# bootstrap); Panel B reuses logvar_ppml_table_parts with the moving-block
# bootstrap outer envelope (log_var_eq_set_boot$ppml), exactly as the combined
# inference panels. Emits the paper's scaled decimal-aligned tabular; the paper
# supplies the float, caption, notes, and the dual \label. Writes
# structural_var_inference.tex + standalone. Run via run_pipeline.R after
# the estimator pages (needs set_id_mean_eq, set_id_boot, log_var_eq_set_boot).

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "structural_var_inference.R"))
paper_source_once(paper_path("mean_equation", "tables", "structural_table_parts.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))

# Panel A: mean equation (keeps its R^2 and N tail).
panel_a <- structural_equation_table_parts(set_id_mean_eq, set_id_boot, n_pc)

# Panel B: PPML log-variance with the set-endpoint bootstrap envelope beneath
# each tau > 0 set cell. The R^2 row stays and renders as "--".
panel_b <- logvar_ppml_table_parts(
  paper_logvar_result("ppml"),
  set_id_mean_eq$tau_display,
  n_pc_r,
  se_type = logvar_ppml_se_type,
  envelope = log_var_eq_set_boot$ppml,
  point_stat = logvar_boot_point_stat(log_var_eq_set_boot, "ppml")
)
combined_table <- paper_structural_var_inference_table(panel_a, panel_b)
publish_latex_artifact(
  "structural_var_inference_table", combined_table,
  packages = c(
    "\\usepackage{dcolumn}", "\\usepackage{setspace}",
    "\\usepackage{amsmath}"
  )
)

cat(sprintf(
  "combined inference table: Panel A (N = %d) over Panel B PPML (N = %d)\n",
  set_id_mean_eq$sample$n, panel_b$n_obs
))

rm(panel_a, panel_b, combined_table)
