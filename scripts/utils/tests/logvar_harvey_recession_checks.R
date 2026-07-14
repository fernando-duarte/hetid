# Recession-certificate and existence checks for the Harvey estimator
# (scripts-paper/log_var_eq_harvey_recession.R): the normalized cone certificate
# separates the full-rank negative recession, the zero-rate unresolved case, and
# the all-zero fail-closed, and the direct two-column objective reproduces the
# pinned recession-rate sweep. Split from logvar_harvey_solver_checks.R for the
# line cap; sourced after it, reusing its hs_try / hs_fx / hs_x helpers.

# Recession certificate and existence block ----------------------------------

check("hs the full-rank counterexample certifies a negative recession", hs_try({
  cert <- logvar_harvey_recession_certificate(hs_fx("y_rec", "y_neg"), hs_fx("x_rec", "x_neg"))
  fit <- logvar_harvey_fit_response(hs_fx("y_rec", "y_neg"), hs_fx("x_rec", "x_neg"))
  identical(cert$classification, "negative_recession") &&
    identical(fit$fit_status, "nonexistence") &&
    identical(fit$diagnostics$error_class, "negative_recession")
}))

check("hs the direct two-column objective reproduces the recession rates", hs_try({
  x2 <- rbind(c(1, 0), c(1, 1), c(1, 100))
  y2 <- c(1, 1, 0)
  d <- c(1, -1)
  want <- c(1, -48.316, -489.500, -4899.5)
  got <- vapply(c(0, 1, 10, 100), function(cc) {
    logvar_harvey_objective(cc * d, y2, x2)
  }, numeric(1))
  max(abs(got - want)) <= 1e-2
}))

check("hs a zero-rate response is unresolved, never nonexistence", hs_try({
  cert <- logvar_harvey_recession_certificate(hs_fx("y_zr", "y_zrate"), hs_fx("x_zr", "x_zrate"))
  fit <- logvar_harvey_fit_response(hs_fx("y_zr", "y_zrate"), hs_fx("x_zr", "x_zrate"))
  identical(cert$classification, "zero_recession") &&
    identical(fit$fit_status, "nonconvergence") &&
    identical(fit$diagnostics$error_class, "zero_recession_unresolved")
}))

# the dossier sanctions either verdict here: a certified collapse direction
# is genuine nonexistence, an uncertified one fails closed as nonconvergence
check("hs rank-deficient positive rows fail closed without huge coefficients", hs_try({
  fit <- logvar_harvey_fit_response(hs_fx("y_rd", "y_rankdef"), hs_fx("x_rd", "x_rankdef"))
  fit$fit_status %in% c("nonconvergence", "nonexistence") && !isTRUE(fit$converged)
}))

check("hs an all-zero response fails closed as a negative intercept recession", hs_try({
  fit <- logvar_harvey_fit_response(numeric(nrow(hs_x)), hs_x)
  identical(fit$fit_status, "nonexistence") &&
    identical(fit$diagnostics$error_class, "negative_recession_all_zero")
}))
