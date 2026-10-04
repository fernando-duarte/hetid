lv_set_grid_scan <- function(mesh, w1, w2, projection, chunk, prep) {
  lv_set_assert(nrow(mesh) > 0L)
  n_coef <- nrow(projection)
  best_min <- rep(Inf, n_coef)
  best_max <- rep(-Inf, n_coef)
  arg_min <- arg_max <- matrix(NA_real_, n_coef, ncol(mesh))
  n_failed <- 0L
  n_domain <- 0L
  any_nonpositive <- any_nonnegative <- rep(FALSE, length(w1))
  for (start in seq(1L, nrow(mesh), by = chunk)) {
    rows <- start:min(start + chunk - 1L, nrow(mesh))
    eps <- w1 - w2 %*% t(mesh[rows, , drop = FALSE])
    any_nonpositive <- any_nonpositive | (rowSums(eps <= 0) > 0)
    any_nonnegative <- any_nonnegative | (rowSums(eps >= 0) > 0)
    ev <- evaluate_log_projection(prep, mesh[rows, , drop = FALSE], "log",
      jacobian = FALSE
    )
    theta <- ev$coef
    n_failed <- n_failed + sum(ev$status == "numerical_failure")
    n_domain <- n_domain + sum(ev$status == "domain_failure")
    # excluded points provide crossing candidates, never attaining starts
    theta[, ev$status != "ok"] <- NA_real_
    for (j in seq_len(n_coef)) {
      k_min <- which.min(theta[j, ])
      k_max <- which.max(theta[j, ])
      if (length(k_min) && theta[j, k_min] < best_min[j]) {
        best_min[j] <- theta[j, k_min]
        arg_min[j, ] <- mesh[rows[k_min], ]
      }
      if (length(k_max) && theta[j, k_max] > best_max[j]) {
        best_max[j] <- theta[j, k_max]
        arg_max[j, ] <- mesh[rows[k_max], ]
      }
    }
  }
  list(
    min = if (all(is.finite(best_min))) best_min else NULL,
    max = best_max, arg_min = arg_min, arg_max = arg_max,
    cross_grid = which(any_nonpositive & any_nonnegative), n_failed = n_failed,
    n_domain = n_domain
  )
}

lv_set_logols_estimator <- function(sample, control = lv_set_logols_control()) {
  w1 <- sample$w1
  w2 <- sample$w2
  projection <- sample$prep$projection
  groups <- lv_set_logols_groups(sample$prep)
  list(
    metadata = list(
      estimator = "logols", target_functional = "theta_log",
      sample_id = sample$sample_id, smoothness = "smooth", response_scale = "log",
      spec_id = lv_set_spec_id(list(
        control = control,
        preparation = lv_set_hash(sample$prep)
      )),
      cold_start_rtol = control$COLD_START_RTOL
    ),
    coef_labels = rownames(projection), theta_labels = colnames(w2),
    FULL_GRID_SAFETY_CAP = control$FULL_GRID_SAFETY_CAP,
    fit_at_b = function(b, start = NULL, phase = NULL) {
      ev <- evaluate_log_projection(sample$prep, b, "log", jacobian = FALSE)
      status <- c(
        ok = "ok", domain_failure = "domain_failure",
        numerical_failure = "nonfinite_fitted_log_variance"
      )[[ev$status]]
      lv_set_fit_result(
        coef = ev$coef, fit_status = status, converged = TRUE,
        objective = 0, score_norm = 0, convergence_code = 0L,
        diagnostics = ev$diagnostics
      )
    },
    jacobian_at_b = function(b, fit = NULL) {
      ev <- evaluate_log_projection(sample$prep, b, "log")
      if (is.null(ev$jacobian)) matrix(NaN, nrow(projection), ncol(w2)) else ev$jacobian
    },
    coef_objective = function(j) {
      lv_logols_objective(sample$prep, groups, j)
    },
    scan_grid = function(mesh) {
      lv_set_grid_scan(mesh, w1, w2, projection, control$SCAN_CHUNK_SIZE, sample$prep)
    },
    precheck = function(quadratic, theta_table) {
      bounds <- profile_containing_box(theta_table)
      census <- lv_set_crossing_census(quadratic, bounds$lower, bounds$upper, w1, w2, control,
        groups = groups
      )
      census$unresolved_coverage <- lv_log_crossing_coverage(groups, census, rownames(projection))
      census
    },
    sides = function(found, precheck) {
      c(
        lv_log_group_sides(groups, precheck$cross, rownames(projection)),
        precheck[c("unresolved", "zero_rows", "unresolved_coverage")]
      )
    }
  )
}

lv_set_logols_sets <- function(sample, path, bounds, tau_control, control) {
  logols_control <- lv_set_logols_control()
  lv_set_check_lattice(
    2 * control$search$GRID_N - 1, ncol(sample$w2),
    min(control$sets$GRID_POINTS_LIMIT, logols_control$FULL_GRID_SAFETY_CAP)
  )
  map_obj <- lv_set_logols_estimator(sample, logols_control)
  seed <- unname(path$point)
  # the tau = 0 row of the figure is the fit at this point, which is one only
  # when the point lies in the set
  point_feasible <- !is.null(seed) && lv_set_point_feasible(
    lv_set_quadratic(path, tau_control$baseline), seed
  )
  keys <- profile_tau_key(tau_control$display)
  results <- stats::setNames(lapply(seq_along(keys), function(i) {
    lv_set_search(map_obj, lv_set_quadratic(path, tau_control$display[[i]]),
      bounds$theta[[keys[[i]]]],
      seed = seed, cold_start_check = FALSE,
      tau = tau_control$display[[i]], control = control
    )
  }), keys)
  list(
    key = "logols", estimator = map_obj, sample = sample, seed = seed,
    point = if (point_feasible) seed else NULL,
    cache = new.env(parent = emptyenv()), grid_cap = NULL, results = results,
    primary = results, audit = NULL, taus = tau_control$display
  )
}
