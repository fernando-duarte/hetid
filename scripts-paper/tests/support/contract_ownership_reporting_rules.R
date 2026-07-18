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
