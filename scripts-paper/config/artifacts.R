# Canonical generated-artifact manifest for the paper analysis pipeline.
.artifact_groups <- c(
  "1" = "tables/descriptive_statistics",
  "2" = "tables",
  "3" = "figures",
  "4" = "figures/descriptive_statistics",
  "5" = "reports",
  "6" = "diagnostics",
  "7" = "state"
)
.artifact_producers <- c(
  a = "reports/build_descriptive_statistics.R",
  b = "mean_equation/diagnostics/heteroskedasticity/render_table.R",
  c = "log_variance/tables/render_ppml_table.R",
  d = "log_variance/tables/render_harvey_table.R",
  e = "log_variance/tables/render_lad_table.R",
  f = "log_variance/tables/render_panels.R",
  g = "log_variance/tables/render_inference_panels.R",
  h = "mean_equation/tables/render_structural_equation_table.R",
  i = "mean_equation/variance_shares/render_variance_share_table.R",
  j = "mean_equation/inference/compute_bounds_by_tau.R",
  k = "mean_equation/figures/render_projections.R",
  l = "mean_equation/figures/render_region_3d.R",
  m = "log_variance/figures/render_bounds_by_tau.R",
  n = "log_variance/figures/fitted_volatility/run.R",
  o = "log_variance/figures/fitted_volatility/run_lad.R",
  p = "mean_equation/inference/run_bootstrap.R",
  q = "log_variance/inference/run_set_bootstrap.R",
  r = "log_variance/diagnostics/joint_null/run.R",
  s = "log_variance/diagnostics/joint_gmm/pipeline_driver.R",
  t = "log_variance/estimators/lad/run_sets.R",
  u = "log_variance/diagnostics/dynamics/run_gate.R",
  v = paste(
    "log_variance/diagnostics/dynamics/run_gate.R",
    "log_variance/extensions/egarch/run_route.R",
    sep = ";"
  ),
  w = "log_variance/extensions/egarch"
)
.artifact_consumers <- c(
  A = "reports/descriptive_stats.tex",
  B = "paper",
  C = "mean_equation/tables/render_structural_equation_table.R",
  D = "log_variance/tables/render_inference_panels.R",
  E = "tests/diagnostics/joint_null",
  F = "tests/diagnostics/joint_gmm",
  G = "tables;tests",
  H = paste(
    "config/decisions/egarch.R",
    "log_variance/extensions/egarch/run_route.R",
    sep = ";"
  ),
  I = "log_variance/extensions/egarch/run_route.R",
  J = "log_variance/extensions/egarch"
)
.artifact_specs <- c(
  "summary_statistics_table|summary_stats.tex|1|a|A|r",
  "correlations_table|correlations.tex|1|a|A|r",
  "ols_mean_equation_table|ols_mean_eq.tex|1|a|A|r",
  "heteroskedasticity_table|hetero_tests.tex|2|b|B|r",
  "heteroskedasticity_standalone_tex|hetero_tests_standalone.tex|2|b|B|r",
  "heteroskedasticity_standalone_pdf|hetero_tests_standalone.pdf|2|b|B|r",
  "log_variance_ppml_table|log_var_eq.tex|2|c|B|r",
  "log_variance_ppml_standalone_tex|log_var_eq_standalone.tex|2|c|B|r",
  "log_variance_ppml_standalone_pdf|log_var_eq_standalone.pdf|2|c|B|r",
  "log_variance_harvey_table|log_var_eq_harvey.tex|2|d|B|r",
  "log_variance_harvey_standalone_tex|log_var_eq_harvey_standalone.tex|2|d|B|r",
  "log_variance_harvey_standalone_pdf|log_var_eq_harvey_standalone.pdf|2|d|B|r",
  "log_variance_lad_table|log_var_eq_lad_panel.tex|2|e|B|l",
  "log_variance_lad_standalone_tex|log_var_eq_lad_panel_standalone.tex|2|e|B|l",
  "log_variance_lad_standalone_pdf|log_var_eq_lad_panel_standalone.pdf|2|e|B|l",
  "log_variance_panels_table|log_var_eq_panels.tex|2|f|B|r",
  "log_variance_panels_standalone_tex|log_var_eq_panels_standalone.tex|2|f|B|r",
  "log_variance_panels_standalone_pdf|log_var_eq_panels_standalone.pdf|2|f|B|r",
  "log_variance_inference_table|log_var_eq_panels_inference.tex|2|g|B|r",
  "log_variance_inference_standalone_tex|log_var_eq_panels_inference_standalone.tex|2|g|B|r",
  "log_variance_inference_standalone_pdf|log_var_eq_panels_inference_standalone.pdf|2|g|B|r",
  "structural_equation_table|structural_eq.tex|2|h|B|r",
  "structural_equation_standalone_tex|structural_eq_standalone.tex|2|h|B|r",
  "structural_equation_standalone_pdf|structural_eq_standalone.pdf|2|h|B|r",
  "structural_equation_inference_table|structural_eq_inference.tex|2|h|B|r",
  "structural_equation_inference_standalone_tex|structural_eq_inference_standalone.tex|2|h|B|r",
  "structural_equation_inference_standalone_pdf|structural_eq_inference_standalone.pdf|2|h|B|r",
  "variance_share_table|var_share.tex|2|i|B|r",
  "variance_share_standalone_tex|var_share_standalone.tex|2|i|B|r",
  "variance_share_standalone_pdf|var_share_standalone.pdf|2|i|B|r",
  "mean_bounds_figure|set_id_bounds_tau.pdf|3|j|B|r",
  "mean_projections_figure|set_id_projections_sd.pdf|3|k|B|r",
  "mean_region_figure|set_id_region_3d.pdf|3|l|B|r",
  "log_ols_bounds_figure|log_var_eq_bounds_tau_logols.pdf|3|m|B|r",
  "ppml_bounds_figure|log_var_eq_bounds_tau_ppml.pdf|3|m|B|r",
  "harvey_bounds_figure|log_var_eq_bounds_tau_harvey.pdf|3|m|B|r",
  "lad_bounds_figure|log_var_eq_bounds_tau_lad.pdf|3|m|B|l",
  "ppml_fitted_volatility_figure|log_var_eq_fitted_volatility_ppml.pdf|3|n|B|r",
  "harvey_fitted_volatility_figure|log_var_eq_fitted_volatility_harvey.pdf|3|n|B|r",
  "lad_fitted_volatility_figure|log_var_eq_fitted_volatility_lad.pdf|3|o|B|l",
  "descriptive_figures|figures.pdf|4|a|A|r",
  "descriptive_report_tex|descriptive_stats.tex|5|a|B|r",
  "descriptive_report_pdf|descriptive_stats.pdf|5|a|B|r",
  "mean_inference_diagnostics|set_id_inference_diagnostics.csv|6|p|C|r",
  "log_variance_inference_diagnostics|log_var_eq_set_inference_diagnostics.csv|6|q|D|r",
  "joint_null_csv|log_var_eq_joint_null.csv|6|r|E|r",
  "joint_null_rds|log_var_eq_joint_null.rds|6|r|E|r",
  "joint_gmm_csv|log_var_eq_joint_gmm.csv|6|s|F|r",
  "joint_gmm_rds|log_var_eq_joint_gmm.rds|6|s|F|r",
  "lad_closure_diagnostics|log_var_eq_lad_closure.csv|6|t|G|l",
  "mean_bootstrap_draws|set_id_boot_draws.rds|7|p|C|r",
  "log_variance_bootstrap_draws|log_var_eq_set_boot_draws.rds|7|q|D|r",
  "dynamics_gate|log_var_eq_dynamics_gate.rds|7|u|H|r",
  "egarch_status|log_var_eq_egarch_status.rds|7|v|I|r",
  "egarch_pilot_state|log_var_eq_egarch_pilot.rds|7|w|J|e",
  "egarch_results_csv|log_var_eq_egarch_x.csv|6|w|J|e",
  "egarch_results_rds|log_var_eq_egarch_x.rds|6|w|J|e",
  "egarch_bounds_figure|log_var_eq_bounds_tau_egarch_x.pdf|3|w|J|e"
)
.artifact_specs <- do.call(rbind, strsplit(.artifact_specs, "|", fixed = TRUE))
stopifnot(ncol(.artifact_specs) == 6L)
artifact_manifest <- data.frame(
  id = .artifact_specs[, 1L],
  basename = .artifact_specs[, 2L],
  old_path = file.path(out_dir, .artifact_specs[, 2L]),
  new_path = file.path(
    out_dir, .artifact_groups[.artifact_specs[, 3L]], .artifact_specs[, 2L]
  ),
  producer = unname(.artifact_producers[.artifact_specs[, 4L]]),
  consumer = unname(.artifact_consumers[.artifact_specs[, 5L]]),
  status = unname(c(r = "required", l = "conditional_lad", e = "conditional_egarch")[
    .artifact_specs[, 6L]
  ]),
  stringsAsFactors = FALSE
)
rownames(artifact_manifest) <- NULL
rm(.artifact_groups, .artifact_producers, .artifact_consumers, .artifact_specs)

.artifact_scalar <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("%s must be one nonempty string.", what), call. = FALSE)
  }
  x
}
.artifact_record <- function(id) {
  id <- .artifact_scalar(id, "Artifact ID")
  row <- artifact_manifest[artifact_manifest$id == id, , drop = FALSE]
  if (nrow(row) != 1L) stop(sprintf("Unknown artifact ID: %s", id), call. = FALSE)
  row
}
artifact_id <- function(basename) {
  basename <- .artifact_scalar(basename, "Artifact basename")
  id <- artifact_manifest$id[artifact_manifest$basename == basename]
  if (length(id) != 1L) {
    stop(sprintf("Unknown or ambiguous artifact basename: %s", basename),
      call. = FALSE
    )
  }
  id
}
artifact_path <- function(id) .artifact_record(id)$new_path[[1L]]
artifact_basename <- function(id) .artifact_record(id)$basename[[1L]]
artifact_dir <- function(id) dirname(artifact_path(id))
artifact_group_dir <- function(group) {
  group <- .artifact_scalar(group, "Artifact group")
  groups <- unique(sub(paste0("^", out_dir, "/"), "", dirname(artifact_manifest$new_path)))
  if (!group %in% groups) stop(sprintf("Unknown artifact group: %s", group), call. = FALSE)
  file.path(out_dir, group)
}
create_artifact_directories <- function() {
  dirs <- unique(dirname(artifact_manifest$new_path))
  invisible(vapply(dirs, dir.create, logical(1),
    recursive = TRUE,
    showWarnings = FALSE
  ))
}
stopifnot(
  nrow(artifact_manifest) == 58L, !anyNA(artifact_manifest),
  !anyDuplicated(artifact_manifest$id), !anyDuplicated(artifact_manifest$basename),
  all(artifact_manifest$status %in% c(
    "required", "conditional_lad", "conditional_egarch"
  ))
)
