# Shared display-tau set mapping, reference alignment, and result assembly.

logvar_reference_residuals <- function(qtr, mean_equation) {
  rows <- match(qtr, mean_equation$qtr)
  if (anyNA(rows)) {
    stop(
      "Log-variance sample is not contained in mean-equation sample",
      call. = FALSE
    )
  }
  as.numeric(
    stats::residuals(mean_equation$ols_fit)[rows]
  )
}

logvar_map_display_taus <- function(
  taus,
  bounds_tau,
  quadratic_at_tau,
  map_one,
  initial_state = NULL
) {
  keys <- vapply(taus, paper_tau_key, character(1))
  stopifnot(!anyDuplicated(keys))
  records <- stats::setNames(
    vector("list", length(keys)),
    keys
  )
  results <- stats::setNames(
    vector("list", length(keys)),
    keys
  )
  diagnostics <- stats::setNames(
    vector("list", length(keys)),
    keys
  )
  boxes <- stats::setNames(
    vector("list", length(keys)),
    keys
  )
  state <- initial_state

  for (index in seq_along(taus)) {
    tau <- taus[[index]]
    key <- keys[[index]]
    box <- bounds_tau[[key]]
    if (is.null(box)) {
      stop(
        sprintf("Missing mean-equation box for tau %s", key),
        call. = FALSE
      )
    }
    quadratic <- quadratic_at_tau(tau)
    mapped <- map_one(
      tau = tau,
      key = key,
      quadratic = quadratic,
      box = box,
      state = state
    )
    stopifnot(
      is.list(mapped),
      is.list(mapped$result)
    )
    state <- mapped$state
    records[[key]] <- list(
      tau = tau,
      key = key,
      quadratic = quadratic,
      box = box,
      result = mapped$result,
      state = state,
      detail = mapped$detail
    )
    results[[key]] <- mapped$result
    diagnostics[[key]] <- mapped$result$diagnostics
    boxes[[key]] <- box
  }

  list(
    keys = keys,
    records = records,
    results = results,
    diagnostics = diagnostics,
    boxes = boxes,
    final_state = state
  )
}

logvar_engine_tau_mapper <- function(
  estimator,
  b_seed,
  max_grid_points,
  max_fit_evals,
  cache,
  warm_chain = TRUE,
  starts_per_side =
    LOGVAR_SEARCH_CONTROL$primary_starts_per_side,
  cold_start_check =
    LOGVAR_SEARCH_CONTROL$cold_start_check
) {
  function(tau, key, quadratic, box, state) {
    budget_state <- logvar_budget_state(max_fit_evals)
    result <- logvar_engine_set_at_tau(
      estimator,
      quadratic,
      box,
      b_seed = b_seed,
      max_grid_points = max_grid_points,
      max_fit_evals = max_fit_evals,
      starts_per_side = starts_per_side,
      cache = cache,
      budget_state = budget_state,
      extra_starts = if (warm_chain) state else NULL,
      cold_start_check = cold_start_check,
      tau = tau
    )
    list(
      result = result,
      state = if (warm_chain) {
        logvar_bounded_args(result$schema)
      } else {
        NULL
      },
      detail = list(budget_state = budget_state)
    )
  }
}

logvar_set_result_core <- function(
  qtr,
  sample_id,
  coef_labels,
  reference,
  point,
  baseline_tau,
  primary_results,
  final_results,
  w1,
  w2,
  search_seed
) {
  baseline_key <- paper_tau_key(baseline_tau)
  baseline <- final_results[[baseline_key]]$table
  stopifnot(identical(baseline$coef, coef_labels))
  list(
    sample = list(n = length(qtr), span = range(qtr)),
    sample_id = sample_id,
    tau_baseline = baseline_tau,
    table = data.frame(
      coef = coef_labels,
      reference = unname(reference),
      point = unname(point),
      set_lower = baseline$set_lower,
      set_upper = baseline$set_upper,
      status = baseline$status,
      row.names = NULL
    ),
    sets = lapply(final_results, `[[`, "table"),
    schema = lapply(final_results, `[[`, "schema"),
    n_feasible = vapply(
      primary_results,
      `[[`,
      integer(1),
      "n_feasible"
    ),
    counts = lapply(primary_results, `[[`, "diagnostics"),
    w1 = w1,
    w2 = w2,
    search_seed = search_seed
  )
}

logvar_bounds_registry_entry <- function(
  estimator,
  results,
  b_seed,
  engine_opts,
  warm_chain = TRUE
) {
  list(
    estimator = estimator,
    schema = lapply(results, `[[`, "schema"),
    sets = lapply(results, `[[`, "table"),
    b_seed = b_seed,
    warm_chain = warm_chain,
    engine_opts = engine_opts,
    output_path = logvar_bounds_tau_path(
      estimator$metadata
    )
  )
}
