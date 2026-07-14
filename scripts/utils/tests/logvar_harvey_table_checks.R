# Rendering checks for the dedicated Harvey MLE/QMLE table. The fixture keeps
# the table path offline while pinning the marker block, coefficient mapping,
# exact display-tau lookup, and estimator-specific notes.

hvt_coef <- c("(Intercept)", "l.pc1")
hvt_set <- function(lo, hi) {
  data.frame(
    coef = hvt_coef, set_lower = lo, set_upper = hi,
    status = "bounded", stringsAsFactors = FALSE
  )
}
hvt_harvey <- list(
  table = data.frame(
    coef = hvt_coef, reference = c(-1.3, 0.2), point = c(-1.2, 0.18),
    stringsAsFactors = FALSE
  ),
  sets = stats::setNames(
    list(
      hvt_set(c(-1.25, 0.17), c(-1.15, 0.19)),
      hvt_set(c(-1.3, 0.16), c(-1.1, 0.2))
    ),
    sprintf("%.17g", c(0.05, 0.1))
  )
)
hvt_lines <- logvar_harvey_append_panel(
  character(0), hvt_harvey, 12L, c(0.05, 0.1), 0.05, 4000L, 20000L,
  caption = "Dedicated Harvey MLE/QMLE table.",
  label = "tab:log_var_eq_harvey", include_ordering = FALSE
)

check("Harvey table renderer emits one marker-wrapped table", {
  sum(hvt_lines == "% BEGIN LOGVAR PANEL harvey") == 1L &&
    sum(hvt_lines == "% END LOGVAR PANEL harvey") == 1L &&
    sum(hvt_lines == "\\begin{table}[!htbp]") == 1L &&
    sum(hvt_lines == "\\end{table}") == 1L
})
check("Harvey table renderer uses reference, point, and exact tau hulls", {
  any(hvt_lines == paste0(
    "$\\theta^{H}_0$ & -1.300 & -1.200 & ",
    "$[-1.250,\\,-1.150]$ & $[-1.300,\\,-1.100]$ \\\\"
  )) &&
    any(hvt_lines == paste0(
      "$\\theta^{H}_{1,R}$ & 0.200 & 0.180 & ",
      "$[0.170,\\,0.190]$ & $[0.160,\\,0.200]$ \\\\"
    )) &&
    any(hvt_lines == "$N$ & 12 & 12 & 12 & 12 \\\\")
})
check("Harvey table renderer includes the MLE and zero-safe disclosures", {
  notes <- paste(hvt_lines, collapse = " ")
  grepl("Gaussian multiplicative-variance MLE/QMLE", notes, fixed = TRUE) &&
    grepl("Zero squared residuals are handled", notes, fixed = TRUE) &&
    !grepl("No PPML standard errors", notes, fixed = TRUE) &&
    !grepl("appended after that ordered pair", notes, fixed = TRUE)
})
check("Harvey standalone renderer uses dedicated context and headers", {
  any(hvt_lines == "\\caption{Dedicated Harvey MLE/QMLE table.}") &&
    any(hvt_lines == "\\label{tab:log_var_eq_harvey}") &&
    any(hvt_lines == paste0(
      " & Reference & $\\tau{=}0$ & $\\tau{=}0.05$ & $\\tau{=}0.1$ \\\\"
    ))
})

hvt_se_frame <- function(se_col) {
  data.frame(
    coef = hvt_coef, expected = se_col, observed = se_col, opg = se_col,
    robust = se_col, hac = c(0.65, 0.05), check.names = FALSE
  )
}
hvt_harvey_se <- c(hvt_harvey, list(se = list(
  reference = hvt_se_frame(c(0.5, 0.05)),
  point = hvt_se_frame(c(0.6, 0.05)),
  hac_lags = 4L
)))
hvt_lines_se <- logvar_harvey_append_panel(
  character(0), hvt_harvey_se, 12L, c(0.05, 0.1), 0.05, 4000L, 20000L,
  caption = "Dedicated Harvey MLE/QMLE table.", label = "tab:log_var_eq_harvey",
  include_ordering = FALSE, se_type = "hac", se_hac_lags = 4L
)

check("Harvey panel renders hac t-stats/stars and selects the hac column", {
  joined <- paste(hvt_lines_se, collapse = "\n")
  # reference theta_0 = -1.3 / hac se 0.65 = -2.00 -> ** (observed 0.5 -> ***)
  grepl("-1.300$^{**}$", joined, fixed = TRUE) &&
    !grepl("-1.300$^{***}$", joined, fixed = TRUE) &&
    grepl("(-2.00)", joined, fixed = TRUE)
})

check("Harvey panel keeps set columns free of statistic cells", {
  # the theta_0 statistic row carries the two point-column t's and blank set
  # cells; pin it to confirm no endpoint statistics were emitted
  any(hvt_lines_se == " & (-2.00) & (-1.85) &  &  \\\\")
})

check("Harvey panel stays blank when se_type is NULL (back-compat)", {
  # hvt_lines (built earlier without se_type) has blank statistic rows
  any(hvt_lines == "$N$ & 12 & 12 & 12 & 12 \\\\") &&
    !any(grepl("(-2.00)", hvt_lines, fixed = TRUE))
})

check("Harvey notes describe the hac SE variation and keep the caveat", {
  notes <- paste(hvt_lines_se, collapse = " ")
  grepl("Newey", notes) && grepl("QMLE", notes) &&
    grepl("condition on the plug-in", notes) &&
    !grepl("No Harvey standard errors", notes, fixed = TRUE)
})

rm(
  hvt_coef, hvt_set, hvt_harvey, hvt_lines, hvt_se_frame, hvt_harvey_se,
  hvt_lines_se
)
