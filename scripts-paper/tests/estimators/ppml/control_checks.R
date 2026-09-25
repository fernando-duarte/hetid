# The adapter verifies scientific identity before delegating the numerical solve.
check("PPML rejects a different family instead of silently substituting it", {
  control <- LOGVAR_PPML_CONTROL
  control$family <- "gaussian"
  error <- tryCatch(logvar_ppml_fit_response(ppml_fx$y, ppml_fx$x_mat,
    control = control
  ), error = identity)
  inherits(error, "error") && grepl("PPML control family must equal quasipoisson",
    conditionMessage(error),
    fixed = TRUE
  )
})
check("PPML rejects disabled acceptance gates", {
  all(vapply(c("rank_switch", "finite_mean_switch", "boundary_switch"), function(key) {
    control <- LOGVAR_PPML_CONTROL
    control[[key]] <- FALSE
    error <- tryCatch(logvar_ppml_fit_response(ppml_fx$y, ppml_fx$x_mat,
      control = control
    ), error = identity)
    inherits(error, "error") && grepl(paste0("PPML control ", key, " must equal TRUE"),
      conditionMessage(error),
      fixed = TRUE
    )
  }, logical(1)))
})
