# Rendering checks for the conditional-median page of the per-estimator
# document: the LAD cell policy is two decimals where the generic log-variance
# policy is three, so both are pinned here.

ladt_coef <- c("(Intercept)", "l.pc1")
ladt_set <- data.frame(
  coef = ladt_coef,
  set_lower = c(-1.256, 0.124),
  set_upper = c(-1.204, 0.176),
  status = "bounded",
  stringsAsFactors = FALSE
)
ladt_result <- list(
  table = data.frame(
    coef = ladt_coef,
    reference = c(-1.234, 0.126),
    point = c(-1.236, 0.137),
    stringsAsFactors = FALSE
  ),
  sets = stats::setNames(
    list(ladt_set),
    paper_tau_key(0.05)
  )
)
ladt_parts <- logvar_estimator_panel_parts(
  ladt_result,
  12L,
  0.05,
  LOGVAR_LAD_PANEL_SPEC,
  NULL, NULL, NULL,
  PAPER_REPORTING_CONTROL$cells$lad,
  NULL
)
ladt_row <- function(i) {
  paste(
    vapply(ladt_parts$columns, function(col) col[[i]], character(1)),
    collapse = " & "
  )
}

check("LAD table cells use the dedicated two-decimal policy", {
  identical(ladt_parts$rows[[1L]], "$\\theta^{0.5}_0$") &&
    identical(ladt_row(1L), "-1.23 & -1.24 & $[-1.26,\\,-1.20]$") &&
    identical(ladt_parts$rows[[3L]], "$\\theta^{0.5}_{1,R}$") &&
    identical(ladt_row(3L), "0.13 & 0.14 & $[0.12,\\,0.18]$")
})

check("generic log-variance cells retain three decimals", {
  identical(logvar_fmt(-1.234), "-1.234") &&
    identical(
      set_cell(-1.256, -1.204, "bounded"),
      "$[-1.256,\\,-1.204]$"
    )
})

rm(ladt_coef, ladt_set, ladt_result, ladt_parts, ladt_row)
