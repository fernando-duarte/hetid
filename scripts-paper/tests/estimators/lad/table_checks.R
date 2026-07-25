# Rendering checks for the dedicated conditional-median table.

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
ladt_lines <- logvar_lad_build_fragment(
  ladt_result,
  12L,
  0.05,
  label = "tab:lad_precision"
)

check("LAD table cells use the dedicated two-decimal policy", {
  any(ladt_lines == paste0(
    "$\\theta^{0.5}_0$ & -1.23 & -1.24 & ",
    "$[-1.26,\\,-1.20]$ \\\\"
  )) &&
    any(ladt_lines == paste0(
      "$\\theta^{0.5}_{1,R}$ & 0.13 & 0.14 & ",
      "$[0.12,\\,0.18]$ \\\\"
    ))
})

check("generic log-variance cells retain three decimals", {
  identical(fmt(-1.234), "-1.234") &&
    identical(
      set_cell(-1.256, -1.204, "bounded"),
      "$[-1.256,\\,-1.204]$"
    )
})

rm(ladt_coef, ladt_set, ladt_result, ladt_lines)
