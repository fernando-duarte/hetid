audit_calls <- list()
audit_engine <- function(estimator, quadratic, box, ...) {
  args <- list(...)
  audit_calls[[length(audit_calls) + 1L]] <<- args
  if (identical(args$tau, 0.20)) {
    stop("audit fixture failure")
  }
  list(schema = data.frame(tau = args$tau))
}
audit_taus <- c(0.10, 0.20, 0.30)
audit_keys <- vapply(
  audit_taus,
  paper_tau_key,
  character(1)
)
audit_boxes <- stats::setNames(
  lapply(audit_taus, function(tau) data.frame(tau = tau)),
  audit_keys
)
audit_results <- logvar_audit_display_taus(
  estimator = list(id = "stub"),
  taus = audit_taus,
  boxes = audit_boxes,
  seed = c(0, 0),
  grid_cap = 17L,
  fit_budget = 23L,
  quadratic_at_tau = function(tau) list(tau = tau),
  engine_fn = audit_engine
)
check("audit runner preserves keys and captures failures", {
  identical(names(audit_results), audit_keys) &&
    isTRUE(audit_results[[audit_keys[[1L]]]]$ok) &&
    !isTRUE(audit_results[[audit_keys[[2L]]]]$ok) &&
    identical(
      audit_results[[audit_keys[[2L]]]]$error,
      "audit fixture failure"
    )
})
check("audit runner owns one cache and fresh per-tau budgets", {
  identical(
    audit_calls[[1L]]$cache,
    audit_calls[[2L]]$cache
  ) &&
    identical(
      audit_calls[[2L]]$cache,
      audit_calls[[3L]]$cache
    ) &&
    !identical(
      audit_calls[[1L]]$budget_state,
      audit_calls[[2L]]$budget_state
    ) &&
    all(vapply(audit_calls, function(call) {
      call$max_grid_points == 17L &&
        call$budget_state$max_fit_evals == 23L
    }, logical(1)))
})
rm(
  audit_calls,
  audit_engine,
  audit_taus,
  audit_keys,
  audit_boxes,
  audit_results
)
