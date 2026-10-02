# Combined inference table stacking the mean equation (Panel A) over the PPML
# log-variance equation (Panel B) under one shared OLS / tau column header:
#   Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1}   (Panel A)
#   E[eps_{t+1}^2 | PC_{R,t}] = exp(theta_0 + PC_{R,t}' theta_R)         (Panel B)
# Both panels come from the builders, estimates and unified bootstrap stage behind
# the PPML estimator page: Panel A from structural_equation_table_parts (mean
# sets and their endpoint bootstrap), Panel B from logvar_ppml_table_parts with
# the set-endpoint envelope (log_var_eq_set_boot$ppml). The adjacent template
# renders the paper's scaled decimal-aligned tabular; the paper supplies the
# float, caption, notes, and the dual \label. Writes structural_var_inference.tex
# + standalone. Run via run_pipeline.R after the bootstrap stage (needs
# set_id_mean_eq, set_id_boot, log_var_eq_set_boot).

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "structural_var_inference.R"))
paper_source_once(paper_path("mean_equation", "tables", "structural_table_parts.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))
paper_source_once(paper_path("log_variance", "tables", "ppml_table_parts.R"))

local({
  panel_a <- structural_equation_table_parts(set_id_mean_eq, set_id_boot, n_pc)
  panel_b <- logvar_ppml_table_parts(
    paper_logvar_result("ppml"),
    set_id_mean_eq$tau_display,
    n_pc_r,
    se_type = logvar_ppml_se_type,
    envelope = log_var_eq_set_boot$ppml,
    point_stat = logvar_boot_point_stat(log_var_eq_set_boot, "ppml")
  )
  publish_latex_artifact(
    "structural_var_inference_table",
    paper_structural_var_inference_table(panel_a, panel_b),
    packages = c(
      "\\usepackage{dcolumn}", "\\usepackage{setspace}",
      "\\usepackage{amsmath}"
    )
  )
  cat(sprintf(
    "combined inference table: Panel A (N = %d) over Panel B PPML (N = %d)\n",
    set_id_mean_eq$sample$n, panel_b$n_obs
  ))
})
