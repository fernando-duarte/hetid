# Ownership rules added by the SSOT/DRY audit.

removed <- c(
  "support/identification/inputs_and_alignment.R",
  "support/identification/residual_construction.R"
)
if (any(file.exists(file.path(paper_root, removed)))) {
  violations <- c(violations, "removed identification modules exist")
}
forbid(
  "artifact lifecycle status outside its manifest owner",
  '"(required|conditional_lad|conditional_egarch)"',
  "config/artifact_manifest_data.R"
)
forbid(
  "presentation precision outside reporting control",
  paste0(
    "digits = [23]L?|",
    'sprintf\\("[^"]*%[.]2g|',
    "signif\\([^\\n]*, 3\\)"
  ),
  c(
    "config/reporting.R",
    "config/figure_rendering.R",
    "mean_equation/figures/render_region_3d.R"
  )
)
forbid(
  "joint-null objective chunk outside its protocol",
  "5000L",
  c(
    "log_variance/diagnostics/protocols.R",
    "log_variance/estimators/controls.R"
  )
)
forbid(
  "joint-null replication tolerance multipliers outside protocol",
  paste0(
    "64 \\* \\.Machine\\$double\\.eps|",
    "4 \\* root_tol\\^2"
  )
)
forbid(
  "coverage-specific percentile field",
  "\\bp05\\b|\\bp95\\b|width_p0?5|width_p95"
)
forbid(
  "reconstructed model-axis prefix",
  'paste0\\("(expected_sdf_pc|l[.]pc|b[0-9]+_|theta|d)"',
  "config/analysis_contract.R"
)
forbid(
  "direct usable-fit predicate",
  paste0(
    'fit_status[^\\n]*==[[:space:]]*"ok"|',
    'identical\\([^\\n]*fit_status[^\\n]*,[[:space:]]*"ok"'
  )
)
forbid(
  "reconstructed registered table basename",
  "paste0\\([^\\n]*(_standalone|[.]tex)"
)
forbid(
  "unregistered engine phase",
  'phase = "(scan|probe|refinement|extra_start|polish|cold_start)"'
)
forbid(
  "unsupported publication sensitivity claim",
  paste0(
    "moved no endpoint|",
    "Results are nearly identical with 15- or 8-quarter"
  )
)
forbid(
  "secondary tablenotes renderer",
  "\\\\\\\\begin\\{tablenotes\\}",
  "support/latex/table_environment.R"
)
forbid(
  "direct SVG device lifecycle",
  "grDevices::svg\\(",
  c(
    "support/graphics/device.R",
    "mean_equation/figures/render_region_3d.R",
    "mean_equation/figures/render_projections.R"
  )
)
forbid(
  "secondary typed-row binder",
  paste0(
    "[.]jg_flatten|[.]logvar_joint_null_flatten|",
    "do[.]call\\(rbind,[[:space:]]*lapply\\(rows,[[:space:]]*",
    "(logvar_joint_gmm_csv_row|[.]jn_assemble_row)"
  ),
  "support/artifacts/diagnostic_schema.R"
)
forbid(
  "secondary bounded-argument collector",
  "arg_lower\\[.*lower_status|arg_upper\\[.*upper_status",
  "log_variance/estimators/shared.R"
)
forbid(
  "secondary estimator map summary",
  "sample_id:|tau = .*status=",
  "log_variance/tables/console_formatting.R"
)
