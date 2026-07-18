# Execution-control identity checks for the PPML estimator.

ppml_control_estimator <- function(
  control = LOGVAR_PPML_CONTROL,
  digits = getOption("digits")
) {
  old_digits <- getOption("digits")
  on.exit(options(digits = old_digits), add = TRUE)
  options(digits = digits)
  logvar_ppml_estimator(
    ppml_fx$w1,
    ppml_fx$w2,
    ppml_fx$pcr,
    ppml_fx$qtr,
    b_point = ppml_fx$b_ref,
    scale_anchor_b = c(0.2, -0.1),
    scale_anchor_source = "lewbel_point",
    control = control
  )
}

check("PPML metadata carries the exact executed control", tryCatch(
  identical(
    ppml_control_estimator()$metadata$fit_control,
    LOGVAR_PPML_CONTROL
  ),
  error = function(e) FALSE
))

check("changing one PPML execution control changes spec_id", tryCatch(
  {
    changed <- LOGVAR_PPML_CONTROL
    changed$glm_maxit <- changed$glm_maxit + 1L
    !identical(
      ppml_control_estimator()$metadata$spec_id,
      ppml_control_estimator(changed)$metadata$spec_id
    )
  },
  error = function(e) FALSE
))

check("PPML spec_id is invariant to options(digits)", tryCatch(
  identical(
    ppml_control_estimator(digits = 4L)$metadata$spec_id,
    ppml_control_estimator(digits = 15L)$metadata$spec_id
  ),
  error = function(e) FALSE
))
