lv_set_search <- function(estimator, quadratic, theta_table, seed = NULL,
                          extra_starts = NULL, max_grid_points = NULL,
                          max_fit_evals = Inf, cache = NULL, budget = NULL,
                          cold_start_check = NULL, tau = NA_real_,
                          starts_per_side = NULL, grid_selector = NULL,
                          control = log_variance_search_control()) {
  lv_set_validate_search(
    estimator, quadratic, theta_table, seed,
    max_grid_points, max_fit_evals, starts_per_side, control
  )
  metadata <- estimator$metadata
  ctrl <- control$search
  if (is.null(budget)) budget <- lv_set_budget(max_fit_evals)
  if (is.null(cache)) cache <- new.env(parent = emptyenv())
  if (is.null(cold_start_check)) cold_start_check <- ctrl$cold_start_check
  if (is.null(starts_per_side)) starts_per_side <- ctrl$primary_starts_per_side
  lv_set_validate_overrides(cold_start_check, tau, cache, budget)
  lv_set_check_extra_starts(extra_starts, theta_table$coef)
  estimator$theta_labels <- theta_table$coef
  lv_set_assert(
    is.function(estimator$fit_at_b), is.function(estimator$jacobian_at_b),
    identical(metadata$smoothness, "smooth"), !is.null(estimator$coef_labels),
    length(starts_per_side) == 1L, starts_per_side >= 1L
  )
  lv_set_cache_bind(cache, metadata)
  omega <- profile_constraint_scales(
    quadratic, profile_theta_scale(quadratic),
    control$sets
  )
  evaluate <- lv_set_evaluator(estimator, cache, budget)
  check_feasible <- function(b) {
    values <- profile_constraint_values(b, quadratic, omega)
    list(
      feasible = max(values) <= control$sets$admission_tolerance,
      max_violation = max(values)
    )
  }
  state <- new.env(parent = emptyenv())
  state$labels <- estimator$coef_labels
  state$n_feasible <- NA_integer_
  state$n_raw_feasible <- NA_integer_
  state$n_failed <- 0L
  state$precheck <- NULL
  selector <- NULL
  diagnostics_of <- function(extra = list()) {
    utils::modifyList(list(
      n_attempted = budget$n_attempted,
      n_evaluated = budget$n_evaluated, n_cached = budget$n_cached,
      n_failed = budget$n_failed, n_raw_feasible = state$n_raw_feasible,
      counters = budget$counters, budget_exhausted = FALSE, selector = selector
    ), extra)
  }
  closed <- function(status, extra = list()) {
    lv_set_result_closed(
      state$labels, status, metadata, tau, quadratic, omega,
      state$n_failed, state$n_feasible, diagnostics_of(extra)
    )
  }
  if (any(theta_table$status != "bounded")) {
    status <- if (any(theta_table$status == "unbounded")) "unbounded" else "unreliable"
    return(closed(status, list(
      closure_reason = paste0("mean_domain_", status),
      mean_status = theta_table$status
    )))
  }
  run <- function() {
    if (!is.null(estimator$precheck)) {
      state$precheck <- estimator$precheck(quadratic, theta_table)
      if (length(state$precheck$zero_rows) > 0L) {
        return(closed("unreliable", list(
          closure_reason = "empty_log_domain", zero_rows = state$precheck$zero_rows
        )))
      }
      if (lv_set_unresolved_precheck(state$precheck)) {
        return(closed("unreliable", list(
          precheck_failed = state$precheck$unresolved,
          unresolved_coverage = state$precheck$unresolved_coverage
        )))
      }
    }
    bounds <- profile_containing_box(theta_table)
    selected <- lv_set_search_grid(
      estimator, quadratic, bounds, seed,
      max_grid_points, grid_selector, state, control
    )
    if (is.null(selected)) {
      return(closed("unreliable"))
    }
    mesh <- selected$grid
    in_lattice_order <- selected$in_lattice_order
    selector <<- selected$selector
    state$n_feasible <- nrow(mesh)
    found <- lv_set_run_scan(
      estimator, mesh, budget, in_lattice_order,
      seed, evaluate, state, starts_per_side, bounds, control
    )
    state$n_failed <- found$n_failed
    if (found$n_failed > 0L) {
      return(closed("unreliable", list(scan_fit_failures = found$n_failed)))
    }
    if (is.null(found$min)) {
      return(closed("unreliable", list(no_successful_fits = TRUE, n_domain = found$n_domain)))
    }
    lv_set_endpoints(
      estimator, quadratic, bounds, seed, extra_starts, cold_start_check,
      tau, evaluate, check_feasible, state, omega, found, diagnostics_of, control
    )
  }
  tryCatch(run(), hetid_error_log_variance_budget = function(e) {
    closed("unreliable", list(budget_exhausted = TRUE, budget_message = conditionMessage(e)))
  })
}

lv_set_search_grid <- function(estimator, quadratic, bounds, seed,
                               max_grid_points, grid_selector, state, control) {
  ctrl <- control$search
  selector <- NULL
  limit <- lv_set_lattice_limit(estimator, control)
  mesh <- lv_set_feasible_grid(
    quadratic, bounds$lower, bounds$upper,
    ctrl$grid_n, control, limit
  )
  if (nrow(mesh) < ctrl$grid_floor) {
    mesh <- lv_set_feasible_grid(
      quadratic, bounds$lower, bounds$upper,
      2 * ctrl$grid_n - 1, control, limit
    )
  }
  # the count as searched, before thinning and the seed rewrite it, since a
  # thin lattice cannot be seen in the count after
  state$n_raw_feasible <- nrow(mesh)
  if (nrow(mesh) == 0L) {
    state$n_feasible <- 0L
    return(NULL)
  }
  in_lattice_order <- FALSE
  if (!is.null(grid_selector)) {
    selected <- grid_selector(mesh, max_grid_points)
    selected <- lv_set_check_selector(selected, mesh)
    selector <- list(
      selector_id = selected$selector_id,
      traversal = "as_selected", n_input = nrow(mesh),
      n_output = nrow(selected$grid)
    )
    mesh <- selected$grid
    in_lattice_order <- TRUE
  } else {
    mesh <- lv_set_coarsen_grid(mesh, max_grid_points)
    # the ordering is quadratic in the grid, so an uncapped scan of a large
    # lattice fit by fit is refused rather than left to run
    if (is.null(estimator$scan_grid) && is.null(max_grid_points) &&
      nrow(mesh) > ctrl$nearest_neighbor_limit) {
      lv_set_stop("The nearest-neighbour scan has ", nrow(mesh),
        " points, set max_grid_points.",
        call. = FALSE
      )
    }
  }
  if (!is.null(seed) && lv_set_point_feasible(quadratic, seed)) {
    mesh <- rbind(mesh, seed)
  }

  list(grid = mesh, in_lattice_order = in_lattice_order, selector = selector)
}

lv_set_run_scan <- function(estimator, mesh, budget, in_lattice_order,
                            seed, evaluate, state, starts_per_side, bounds, control) {
  ctrl <- control$search
  if (!is.null(estimator$scan_grid)) {
    if (budget$n_evaluated + nrow(mesh) > budget$max_fit_evals) {
      lv_set_budget_stop("scan", sprintf(
        "a scan of %d points exceeds max_fit_evals = %s",
        nrow(mesh), format(budget$max_fit_evals)
      ))
    }
    found <- lv_set_check_scan(estimator$scan_grid(mesh), estimator)
    budget$counters[["scan"]] <- budget$counters[["scan"]] + nrow(mesh)
    budget$n_attempted <- budget$n_attempted + nrow(mesh)
    budget$n_evaluated <- budget$n_evaluated + nrow(mesh)
    budget$n_failed <- budget$n_failed + found$n_failed
    found
  } else {
    visit <- if (in_lattice_order) {
      seq_len(nrow(mesh))
    } else {
      lv_set_order_grid(mesh, seed)
    }
    lv_set_scan(mesh, visit, evaluate, state,
      pool_k = starts_per_side,
      separation = ctrl$start_separation_fraction * sqrt(sum((bounds$upper - bounds$lower)^2))
    )
  }
}
