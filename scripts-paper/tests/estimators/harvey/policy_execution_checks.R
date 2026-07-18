# Execution checks for the serialized Harvey start and scaling policies.

check("Harvey fit-stage policy can skip the PPML-at-b rung", hs_try({
  control <- LOGVAR_HARVEY_CONTROL
  control$fit_stage_policy <- c("warm", "standalone")
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  est <- logvar_harvey_estimator(
    fx$w1,
    hs_w2,
    fx$pcr,
    fx$qtr,
    ppml_start_at_b = hs_fake_ppml(counter),
    ppml_start_at_b_source_id = "fixture-ppml-v1",
    control = control
  )
  fit <- est$fit_at_b(hs_b, start = hs_inf)
  counter$n == 0L && logvar_harvey_accepted(fit)
}))

check("Harvey standalone policy can disable every fallback start", hs_try({
  control <- LOGVAR_HARVEY_CONTROL
  control$fit_stage_policy <- "standalone"
  control$standalone_start_policy <- character(0)
  est <- logvar_harvey_estimator(
    fx$w1,
    hs_w2,
    fx$pcr,
    fx$qtr,
    control = control
  )
  !logvar_harvey_accepted(est$fit_at_b(hs_b))
}))

check("Harvey rejects an unsupported serialized scaling policy", {
  control <- LOGVAR_HARVEY_CONTROL
  control$scaling_policy <- "response_standardization"
  inherits(
    try(harvey_control_estimator(control), silent = TRUE),
    "try-error"
  )
})
