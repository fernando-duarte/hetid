# Shared fresh-cache audit runner for estimator set maps. Each invocation owns
# one cache, while every tau receives a fresh fit budget. Failures are recorded
# as data so estimator-specific reconciliation can demote results atomically.

logvar_audit_display_taus <- function(
  estimator,
  taus,
  boxes,
  seed,
  grid_cap,
  fit_budget,
  quadratic_at_tau,
  grid_selector = NULL,
  starts_per_side = LOGVAR_SEARCH_CONTROL$audit_starts_per_side,
  cold_start_check = LOGVAR_SEARCH_CONTROL$cold_start_check,
  engine_fn = logvar_engine_set_at_tau
) {
  stopifnot(
    is.list(estimator),
    is.function(quadratic_at_tau),
    is.function(engine_fn),
    length(taus) > 0L,
    length(starts_per_side) == 1L,
    starts_per_side >= 1L
  )
  keys <- vapply(taus, paper_tau_key, character(1))
  stopifnot(
    !anyDuplicated(keys),
    all(keys %in% names(boxes))
  )
  cache <- new.env(parent = emptyenv())
  results <- stats::setNames(vector("list", length(keys)), keys)
  for (index in seq_along(taus)) {
    tau <- taus[[index]]
    key <- keys[[index]]
    budget <- logvar_budget_state(fit_budget)
    results[[key]] <- tryCatch(
      list(
        ok = TRUE,
        res = engine_fn(
          estimator,
          quadratic_at_tau(tau),
          boxes[[key]],
          b_seed = seed,
          max_grid_points = grid_cap,
          starts_per_side = starts_per_side,
          cache = cache,
          budget_state = budget,
          grid_selector = grid_selector,
          cold_start_check = cold_start_check,
          tau = tau
        )
      ),
      error = function(error) {
        list(
          ok = FALSE,
          error = conditionMessage(error)
        )
      }
    )
  }
  results
}
