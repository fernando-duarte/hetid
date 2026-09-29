# Combined inference table stacking the mean equation (Panel A) over the PPML
# log-variance equation (Panel B) under one shared OLS / tau column header:
#   Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1}   (Panel A)
#   E[eps_{t+1}^2 | PC_{R,t}] = exp(theta_0 + PC_{R,t}' theta_R)         (Panel B)
# Every number comes from the structural-inference calculation
# (paper_structural_inference_run): its OLS reference, tau = 0 points with their
# bootstrap statistics, and tau > 0 public-box and sampled PPML sets with their
# pointwise bootstrap intervals. Its tidy rows become the two panels, which the
# existing template renders into the paper's scaled decimal-aligned tabular; the
# paper supplies the float, caption, notes, and the dual \label. Writes
# structural_var_inference.tex + standalone. Run via run_pipeline.R after the
# data preparation.

paper_source_once(paper_path("support", "structural_inference", "api.R"))
paper_source_once(paper_path("support", "structural_inference", "rows.R"))
paper_source_once(paper_path("support", "structural_inference", "panels.R"))
paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "structural_var_inference.R"))

# the calculation result holds every bootstrap draw, so it lives only inside
# this scope and is released before the pipeline moves on
local({
  result <- paper_structural_inference_run()
  if (!isTRUE(result$bootstrap$publication_ok)) {
    stop("The structural inference failed its publication gates.", call. = FALSE)
  }
  panels <- structural_inference_panels(
    structural_inference_rows(result), result$prepared$settings
  )
  publish_latex_artifact(
    "structural_var_inference_table",
    paper_structural_var_inference_table(panels$mean, panels$variance),
    packages = c(
      "\\usepackage{dcolumn}", "\\usepackage{setspace}",
      "\\usepackage{amsmath}"
    )
  )
  cat(sprintf(
    "combined inference table: Panel A (N = %d) over Panel B PPML (N = %d)\n",
    result$prepared$n_obs, result$prepared$variance_n_obs
  ))
})
invisible(gc())
