#!/usr/bin/env Rscript
# The combined inference template's row rendering, its header contract and its
# refusal of unprintable cells, on hand-built panels shaped like the mean and
# PPML table-part builders' output. Run from root:
#   Rscript scripts-paper/tests/support/structural_var_inference_template_checks.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "analysis.R"))
paper_source_once(paper_path("support", "reporting", "cells.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))
paper_source_once(paper_path("support", "latex", "structural_var_inference.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

headers <- c("OLS", "$\\tau{=}0$", "$\\tau{=}0.05$", "$\\tau{=}0.1$", "$\\tau{=}0.2$")
column <- function(top, bottom, r_squared, n_obs) c(top, bottom, r_squared, n_obs)
panel_a <- list(
  headers = headers, row_labels = c("$b_0$", "", "$R^2$", "$N$"),
  columns = list(
    column("0.804***", "(14.52)", "0.10", "256"),
    column("0.796***", "(13.57)", "--", "256"),
    column("$[0.746,\\,0.846]$", "$(0.646,\\,0.946)$", "--", "256"),
    column("$[0.696,\\,0.896]$", "$(0.596,\\,0.996)$", "--", "256"),
    column("$[0.596,\\,0.996]$", "$(0.496,\\,1.096)$", "--", "256")
  )
)
panel_b <- list(
  headers = headers, rows = c("$\\theta_0$", "", "$R^2$", "$N$"),
  columns = list(
    column("$-1.250$***", "($-5.65$)", "--", "255"),
    column("$-1.250$***", "($-5.60$)", "--", "255"),
    column("", "", "--", "255"),
    column("$[-1.300,\\,-1.200]$", "$(-1.400,\\,-1.100)$", "--", "255"),
    column("$[-1.450,\\,-1.050]$", "$(-1.550,\\,-0.950)$", "--", "255")
  )
)
raises <- function(expr, pattern) {
  grepl(pattern, tryCatch(
    {
      force(expr)
      ""
    },
    error = conditionMessage
  ))
}

check("the contract's display taus render to the template's fixed headers", {
  taus <- PAPER_ANALYSIS_CONTRACT$tau$display
  identical(paper_tau_col_headers(taus), headers) &&
    identical(logvar_estimator_headers("OLS", taus), headers)
})
check("a coefficient row renders with x10 scaling and decimal alignment", {
  rows_a <- paper_structural_var_panel_rows(panel_a$row_labels, panel_a$columns)
  any(rows_a == paste0(
    "$b_0$ & \\shiftedestimate{8}{04\\text{***}} & \\shiftedlastestimate{7}{96\\text{***}}",
    " & \\llap{$[$}{7}.46\\intervalcomma & {8}.46\\rlap{$]$}",
    " & \\llap{$[$}{6}.96\\intervalcomma & {8}.96\\rlap{$]$}",
    " & \\llap{$[$}{5}.96\\intervalcomma & {9}.96\\rlap{$]$} \\\\[-2pt]"
  )) && any(rows_a == paste0(
    "$R^2$ & \\shiftedestimate{0}{10} & \\estimatesummary{--} & \\intervalsummary{--}",
    " & \\intervalsummary{--} & \\intervalsummary{--} \\\\"
  ))
})
check("negative cells and a blank degenerate set render in place", {
  rows_b <- paper_structural_var_panel_rows(panel_b$rows, panel_b$columns)
  any(rows_b == paste0(
    "$\\theta_0$ & \\shiftedestimate{-12}{50\\text{***}}",
    " & \\shiftedlastestimate{-12}{50\\text{***}}",
    " &  &  & \\llap{$[$}{-13}.00\\intervalcomma & {-12}.00\\rlap{$]$}",
    " & \\llap{$[$}{-14}.50\\intervalcomma & {-10}.50\\rlap{$]$} \\\\[-2pt]"
  ))
})
check("printable panels raise no refusal", {
  length(paper_structural_var_unprintable(panel_a, panel_a$row_labels, "Panel A")) == 0L &&
    length(paper_structural_var_unprintable(panel_b, panel_b$rows, "Panel B")) == 0L
})
check("a withheld statistic and a half-infinite set are refused by name", {
  broken <- panel_b
  broken$columns[[2L]][[2L]] <- "--"
  broken$columns[[5L]][[1L]] <- "$[-1.450,\\,\\infty)$"
  raises(
    paper_structural_var_inference_table(panel_a, broken),
    paste0(
      "Panel B \\$\\\\theta_0\\$ \\$\\\\tau\\{=\\}0\\$.*",
      "Panel B \\$\\\\theta_0\\$ \\$\\\\tau\\{=\\}0\\.2\\$"
    )
  )
})
check("a panel whose reference header is not OLS stops the table", {
  renamed <- panel_b
  renamed$headers[[1L]] <- "Reference"
  raises(paper_structural_var_inference_table(panel_a, renamed), "headers")
})
.test$finish()
