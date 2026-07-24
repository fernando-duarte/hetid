# Execution-control identity checks for the Harvey estimator.

harvey_control_estimator <- function(
  control = LOGVAR_HARVEY_CONTROL,
  digits = getOption("digits"),
  upstream = list()
) {
  old_digits <- getOption("digits")
  on.exit(options(digits = old_digits), add = TRUE)
  options(digits = digits)
  do.call(logvar_harvey_estimator, c(
    list(
      w1 = harvey_fx$w1,
      w2 = harvey_fx$w2,
      pcr = harvey_fx$pcr,
      qtr = harvey_fx$qtr,
      b_point = harvey_fx$b_ref
    ),
    upstream,
    list(control = control)
  ))
}

harvey_identity_upstream <- function() {
  n_coef <- ncol(harvey_fx$pcr) + 1L
  list(
    ppml_bundle = list(
      coef_original = rep(0, n_coef),
      valid_for = "variance_start",
      source = "fixture_point"
    ),
    ppml_bundle_source_id = "fixture-ppml-point-v1",
    logols_coef = rep(0, n_coef)
  )
}

check("Harvey metadata carries the exact executed control", hs_try({
  identical(
    harvey_control_estimator()$metadata$fit_control,
    LOGVAR_HARVEY_CONTROL
  )
}))

check("changing one Harvey execution control changes spec_id", hs_try({
  changed <- LOGVAR_HARVEY_CONTROL
  changed$maxit <- changed$maxit + 1L
  !identical(
    harvey_control_estimator()$metadata$spec_id,
    harvey_control_estimator(changed)$metadata$spec_id
  )
}))

check("Harvey rejects a noncanonical normal-log-square gap", {
  rejected <- try(
    harvey_control_estimator(upstream = list(
      normal_log_square_gap = LOGVAR_NORMAL_LOG_SQUARE_GAP + 0.1
    )),
    silent = TRUE
  )
  inherits(rejected, "try-error")
})

check("Harvey spec_id is invariant to options(digits)", hs_try({
  upstream <- harvey_identity_upstream()
  identical(
    harvey_control_estimator(digits = 4L, upstream = upstream)$metadata$spec_id,
    harvey_control_estimator(digits = 15L, upstream = upstream)$metadata$spec_id
  )
}))

check("changing the PPML point bundle changes Harvey spec_id", hs_try({
  base <- harvey_identity_upstream()
  changed <- base
  changed$ppml_bundle$coef_original[[1L]] <- 0.25
  !identical(
    harvey_control_estimator(upstream = base)$metadata$spec_id,
    harvey_control_estimator(upstream = changed)$metadata$spec_id
  )
}))

check("changing the PPML bundle source changes Harvey spec_id", hs_try({
  base <- harvey_identity_upstream()
  changed <- base
  changed$ppml_bundle_source_id <- "fixture-ppml-point-v2"
  !identical(
    harvey_control_estimator(upstream = base)$metadata$spec_id,
    harvey_control_estimator(upstream = changed)$metadata$spec_id
  )
}))

check("changing the log-OLS start vector changes Harvey spec_id", hs_try({
  base <- harvey_identity_upstream()
  changed <- base
  changed$logols_coef[[2L]] <- -0.5
  !identical(
    harvey_control_estimator(upstream = base)$metadata$spec_id,
    harvey_control_estimator(upstream = changed)$metadata$spec_id
  )
}))

check("changing the arbitrary-b PPML source changes Harvey spec_id", hs_try({
  base <- list(
    ppml_start_at_b = function(b) NULL,
    ppml_start_at_b_source_id = "fixture-ppml-map-v1"
  )
  changed <- base
  changed$ppml_start_at_b_source_id <- "fixture-ppml-map-v2"
  !identical(
    harvey_control_estimator(upstream = base)$metadata$spec_id,
    harvey_control_estimator(upstream = changed)$metadata$spec_id
  )
}))
