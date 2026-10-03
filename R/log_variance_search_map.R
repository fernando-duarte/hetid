lv_set_display_map <- function(estimator, path, theta_tables, taus, seed, grid_cap,
                               fit_budget, cache, control = log_variance_search_control()) {
  keys <- profile_tau_key(taus)
  lv_set_assert(!anyDuplicated(keys), all(keys %in% names(theta_tables)))
  warm <- NULL
  results <- list()
  for (i in seq_along(taus)) {
    results[[keys[[i]]]] <- lv_set_search(estimator,
      lv_set_quadratic(path, taus[[i]]), theta_tables[[keys[[i]]]],
      seed = seed,
      extra_starts = warm, max_grid_points = grid_cap, cache = cache,
      budget = lv_set_budget(fit_budget), tau = taus[[i]], control = control
    )
    warm <- lv_set_bounded_args(results[[keys[[i]]]]$schema)
  }
  results
}

lv_set_ppml_builder <- function(sample, context,
                                ppml_control = lv_set_ppml_control()) {
  keys <- apply(context$grid, 1L, lv_set_b_key)
  pilot <- lv_set_ppml_pilot(
    sample, context$anchor,
    context$grid[keys != lv_set_b_key(context$anchor), , drop = FALSE], ppml_control
  )
  list(pilot = pilot, build = function() {
    lv_set_ppml_estimator(sample,
      point = context$point, anchor = context$anchor,
      anchor_source = context$anchor_source, response_scale = pilot$response_scale,
      control = ppml_control
    )
  })
}

lv_set_ppml_sets <- function(sample, context, path, bounds, tau_control, control) {
  ctrl <- control$search
  ppml_control <- lv_set_ppml_control()
  builder <- lv_set_ppml_builder(sample, context, ppml_control)
  pilot <- builder$pilot
  build <- builder$build
  map_obj <- build()
  cache <- new.env(parent = emptyenv())
  primary <- lv_set_display_map(
    map_obj, path, bounds$theta, tau_control$display,
    context$seed, ctrl$primary_grid_cap, ctrl$primary_fit_budget, cache, control
  )
  # the audit fits through an estimator of its own, over a larger grid chosen
  # to fill the set, so it shares no fit and no start with the first search
  audit <- lv_set_audit_run(build(), path, bounds$theta, tau_control$display,
    context$seed, ctrl$coverage_grid_cap, ctrl$coverage_fit_budget,
    grid_selector = function(mesh, max_points) {
      lv_set_morton_select(mesh, max_points, ppml_control)
    }, control = control
  )
  reconciled <- lv_set_audit_apply(primary, audit, control,
    selector_id = lv_set_morton_id()
  )
  list(
    key = "ppml", estimator = map_obj, sample = sample, seed = context$seed,
    point = context$point, cache = cache, grid_cap = ctrl$primary_grid_cap,
    results = reconciled$results, primary = primary,
    audit = reconciled$audit, selector_provenance =
      reconciled$selector_provenance, pilot = pilot, taus = tau_control$display
  )
}

lv_set_nesting_repair <- function(results, rerun, tolerance) {
  keys <- names(results)
  columns <- c("tau", "coef", "lower", "upper", "lower_status", "upper_status")
  rows <- function() do.call(rbind, lapply(results, function(r) r$schema[columns]))
  violations <- lv_set_check_nesting(rows(), tolerance)
  if (nrow(violations) > 0L) {
    for (tau in unique(violations$tau)) {
      k <- match(profile_tau_key(tau), keys)
      near <- c(
        if (k > 1L) lv_set_bounded_args(results[[k - 1L]]$schema),
        lv_set_bounded_args(results[[k]]$schema),
        if (k < length(results)) lv_set_bounded_args(results[[k + 1L]]$schema)
      )
      results[[k]] <- rerun(tau, near)
    }
    violations <- lv_set_check_nesting(rows(), tolerance)
    for (i in seq_len(nrow(violations))) {
      k <- match(profile_tau_key(violations$tau[i]), keys)
      j <- match(violations$coef[i], results[[k]]$schema$coef)
      results[[k]]$schema[[paste0(violations$side[i], "_status")]][j] <- "unreliable"
    }
  }
  list(results = results, violations = violations, rows = rows())
}

lv_set_point_rows <- function(estimator, point) {
  fit <- estimator$fit_at_b(point)
  value <- if (log_variance_fit_ok(fit)) {
    fit$coef
  } else {
    stats::setNames(rep(NA_real_, length(estimator$coef_labels)), estimator$coef_labels)
  }
  status <- ifelse(is.finite(value), "bounded", "unreliable")
  data.frame(
    tau = 0, coef = names(value), lower = unname(value), upper = unname(value),
    lower_status = status, upper_status = status, source = "point", row.names = NULL,
    stringsAsFactors = FALSE
  )
}

lv_set_bounds_by_tau <- function(sets, path, bounds,
                                 control = log_variance_search_control()) {
  lv_set_fixed_rng({
    ctrl <- control$search
    estimator <- sets$estimator
    mesh <- bounds$grid
    keys <- profile_tau_key(mesh)
    lv_set_assert(all(keys %in% names(bounds$theta)), all(names(sets$results) %in%
      names(bounds$theta)),
    "the sets are of another sample than the mean fit" =
      identical(estimator$metadata$sample_id, path$sample_id)
    )
    budget <- lv_set_budget()
    thin <- numeric(0)
    run_tau <- function(tau, extra) {
      result <- lv_set_search(estimator, lv_set_quadratic(path, tau),
        bounds$theta[[profile_tau_key(tau)]],
        seed = sets$seed, extra_starts = extra,
        max_grid_points = sets$grid_cap, cache = sets$cache, budget = budget,
        starts_per_side = ctrl$primary_starts_per_side, tau = tau, control = control
      )
      n_raw <- lv_set_path_raw_count(result)
      if (!is.na(n_raw) && n_raw < ctrl$grid_floor) {
        for (column in c("lower_status", "upper_status")) {
          bounded <- result$schema[[column]] == "bounded"
          result$schema[[column]][bounded] <- "unreliable"
        }
        thin <<- union(thin, tau)
      }
      result
    }
    results <- list()
    warm <- NULL
    for (i in seq_along(mesh)) {
      results[[keys[[i]]]] <- run_tau(mesh[[i]], warm)
      warm <- lv_set_bounded_args(results[[keys[[i]]]]$schema)
    }
    repaired <- lv_set_nesting_repair(results, run_tau, ctrl$nesting_rtol)
    results <- repaired$results
    violations <- repaired$violations
    columns <- names(repaired$rows)
    rows <- rbind(
      cbind(repaired$rows, source = "grid"),
      cbind(do.call(rbind, lapply(sets$results, function(r) r$schema[columns])),
        source = "display"
      )
    )
    if (!is.null(sets$point)) rows <- rbind(rows, lv_set_point_rows(estimator, sets$point))
    rownames(rows) <- NULL
    raw_counts <- vapply(results, function(r) {
      as.integer(r$diagnostics$n_raw_feasible)
    }, integer(1))
    diagnostics <- list(
      raw_feasible = raw_counts, thin_lattice = thin,
      nesting_downgrades = violations, cache_hits = budget$counters[["cache_hit"]],
      n_evaluated = budget$n_evaluated,
      box_escapes = sum(vapply(results, function(r) {
        length(r$diagnostics$box_escape)
      }, integer(1)))
    )
    closed <- lv_set_path_closures(results)
    if (length(closed)) diagnostics$pre_grid_closures <- closed
    displayed <- lv_set_path_closures(sets$results)
    if (length(displayed)) diagnostics$display_pre_grid_closures <- displayed
    list(
      rows = rows, estimator = sets$key,
      target_functional = estimator$metadata$target_functional, grid = mesh,
      diagnostics = diagnostics
    )
  })
}
