# Execution-control identity checks for the LAD estimator.

lad_control_estimator <- function(
  control = LOGVAR_LAD_CONTROL,
  digits = getOption("digits")
) {
  old_digits <- getOption("digits")
  on.exit(options(digits = old_digits), add = TRUE)
  options(digits = digits)
  logvar_lad_estimator(
    lad_fx$w1,
    lad_fx$w2,
    lad_fx$pcr,
    lad_fx$qtr,
    control = control
  )
}

qr_check("LAD metadata carries the exact executed control", {
  identical(
    lad_control_estimator()$metadata$fit_control,
    LOGVAR_LAD_CONTROL
  )
})

qr_check("changing one LAD execution control changes spec_id", {
  changed <- LOGVAR_LAD_CONTROL
  changed$probe_max_steps <- changed$probe_max_steps + 1L
  !identical(
    lad_control_estimator()$metadata$spec_id,
    lad_control_estimator(changed)$metadata$spec_id
  )
})

qr_check("LAD spec_id is invariant to options(digits)", {
  identical(
    lad_control_estimator(digits = 4L)$metadata$spec_id,
    lad_control_estimator(digits = 15L)$metadata$spec_id
  )
})
