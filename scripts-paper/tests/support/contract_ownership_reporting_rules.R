# Reporting and diagnostic policy ownership rules.

forbid(
  "secondary coefficient-row interleaver",
  "interleave <- function",
  "support/reporting/inference.R"
)
forbid(
  "secondary Newey-West table implementation",
  "sandwich::NeweyWest\\(",
  "support/reporting/inference.R"
)
forbid(
  "caption-owned reporting defaults",
  "se_type = \"hac\"|se_hac_lags = 4L",
  "config/reporting.R"
)
forbid(
  "literal reporting-policy prose",
  paste0(
    "with 4 lags|10/5/1\\\\%|",
    "p<0\\.01.*p<0\\.05.*p<0\\.10|",
    "p<0\\.10.*p<0\\.05.*p<0\\.01"
  )
)
forbid(
  "secondary diagnostics fitted-ratio cutoff",
  "ratio > 0\\.001|fitted_sd_ratio_cutoff = 0\\.001",
  "config/diagnostics.R"
)
forbid(
  "secondary GQ deflator-position policy",
  "gq_deflator_position = 2L|min\\(2L, ncol\\(z_mat\\)\\)",
  "config/diagnostics.R"
)
forbid(
  "secondary heteroskedasticity rejection-level owner",
  "rejection_level = \"two_stars\"",
  "config/diagnostics.R"
)
forbid(
  "private heteroskedasticity star thresholds",
  "sig <- 0\\.05|stars <- if \\(x < 0\\.01\\)"
)
forbid(
  "literal heteroskedasticity significance legend",
  "p<0\\.10.*p<0\\.05.*p<0\\.01"
)
forbid(
  "literal dynamics explanatory lag",
  "lag-4 screen|lag 1\\.\\.8"
)
forbid(
  "secondary identified-set figure style",
  "#2a78d6|grey35|alpha = 0[.]35|linewidth = 0[.]4",
  "config/figure_rendering.R"
)
forbid(
  "literal production LaTeX label",
  "tab:[A-Za-z0-9_:-]+",
  "config/artifact_latex.R"
)
forbid(
  "secondary LaTeX artifact publisher",
  paste0(
    "write_latex_table <- function|",
    "publish_latex_artifact <- function"
  ),
  "support/latex/artifact_publication.R"
)
forbid(
  "reverse lookup of a registered table artifact",
  "artifact_id\\(paste0\\([^\\n]*[.]tex",
  character(0)
)
forbid(
  "registered LaTeX publication bypass",
  "write_latex_table\\(|compile_latex_pdf\\(",
  c(
    "support/latex/artifact_publication.R",
    "support/latex/table_pipeline.R",
    "reports/build_descriptive_statistics.R"
  )
)
forbid(
  "secondary estimator-panel tau assembler",
  "keys <- vapply\\(tau_display, paper_tau_key",
  "log_variance/tables/estimator_panel.R"
)
forbid(
  "literal coefficient-table style",
  "footnotesize.*tabcolsep.*[0-9]+pt|rule_after = [0-9]+L",
  "config/reporting.R"
)
