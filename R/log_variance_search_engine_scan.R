lv_set_feasible_grid <- function(quadratic, lower, upper, n_axis, control,
                                 raw_limit = control$sets$grid_points_limit) {
  lv_set_check_lattice(n_axis, length(lower), raw_limit)
  axes <- Map(function(lo, hi) seq(lo, hi, length.out = n_axis), lower, upper)
  mesh <- as.matrix(expand.grid(axes, KEEP.OUT.ATTRS = FALSE))
  dimnames(mesh) <- NULL
  omega <- profile_constraint_scales(
    quadratic, profile_theta_scale(quadratic),
    control$sets
  )
  admitted <- profile_constraint_values(mesh, quadratic, omega) <=
    control$sets$admission_tolerance
  # all() row by row, as one column-wise Reduce over `&`
  keep <- Reduce(
    `&`, lapply(seq_len(ncol(admitted)), function(j) admitted[, j]),
    rep(TRUE, nrow(admitted))
  )
  mesh[keep, , drop = FALSE]
}

lv_set_coarsen_grid <- function(mesh, max_points) {
  m <- nrow(mesh)
  if (is.null(max_points) || m <= max_points) {
    return(mesh)
  }
  mesh[seq(1L, m, by = ceiling(m / max_points)), , drop = FALSE]
}

lv_set_order_grid <- function(mesh, seed = NULL) {
  m <- nrow(mesh)
  if (m == 0L) {
    return(integer(0))
  }
  # transposed once, not on each of the m steps
  columns <- t(mesh)
  current <- if (is.null(seed) || anyNA(seed)) 1L else which.min(colSums((columns - seed)^2))
  visit <- integer(m)
  left <- rep(TRUE, m)
  for (k in seq_len(m)) {
    visit[k] <- current
    left[current] <- FALSE
    if (k == m) break
    distance <- colSums((columns - mesh[current, ])^2)
    distance[!left] <- Inf
    current <- which.min(distance)
  }
  visit
}

lv_set_point_precedes <- function(x, y) {
  if (anyNA(y)) {
    return(TRUE)
  }
  differing <- which(unname(x) != unname(y))
  length(differing) > 0L && x[differing[1L]] < y[differing[1L]]
}

lv_set_scan_pools <- function(values, pts, pool_k, separation) {
  point_matrix <- do.call(rbind, pts)
  # base::order by name, do.call would take a caller's variable called order
  ranked <- function(value) {
    do.call(base::order, c(
      list(value),
      lapply(seq_len(ncol(point_matrix)), function(j) point_matrix[, j]),
      list(method = "radix")
    ))
  }
  pick <- function(ranking) {
    kept <- list()
    for (i in ranking) {
      b <- pts[[i]]
      far <- all(vapply(kept, function(a) sqrt(sum((a - b)^2)) > separation, logical(1)))
      if (length(kept) == 0L || far) kept[[length(kept) + 1L]] <- b
      if (length(kept) >= pool_k) break
    }
    kept[-1L]
  }
  n_coef <- ncol(values)
  list(
    arg_min_pool = lapply(seq_len(n_coef), function(j) pick(ranked(values[, j]))),
    arg_max_pool = lapply(seq_len(n_coef), function(j) pick(ranked(-values[, j])))
  )
}

lv_set_scan <- function(mesh, visit, evaluate, state, pool_k, separation) {
  best_min <- best_max <- arg_min <- arg_max <- NULL
  warm <- NULL
  n_failed <- 0L
  values <- list()
  pts <- list()
  for (k in visit) {
    b <- stats::setNames(mesh[k, ], colnames(mesh))
    fit <- evaluate(b, phase = "scan", start = warm)
    if (!log_variance_fit_ok(fit)) {
      n_failed <- n_failed + 1L
      state$n_failed <- state$n_failed + 1L
      next
    }
    warm <- fit$warm_start
    if (is.null(state$labels)) state$labels <- names(fit$coef)
    v <- unname(fit$coef)
    if (pool_k > 1L) {
      values[[length(values) + 1L]] <- v
      pts[[length(pts) + 1L]] <- b
    }
    if (is.null(best_min)) {
      best_min <- rep(Inf, length(v))
      best_max <- rep(-Inf, length(v))
      arg_min <- arg_max <- matrix(NA_real_, length(v), ncol(mesh))
    }
    updated <- lv_set_scan_extremes(v, b, best_min, best_max, arg_min, arg_max)
    best_min <- updated$min
    best_max <- updated$max
    arg_min <- updated$arg_min
    arg_max <- updated$arg_max
  }
  pools <- if (pool_k > 1L && length(values) > 0L) {
    lv_set_scan_pools(do.call(rbind, values), pts, pool_k, separation)
  } else {
    NULL
  }
  list(
    min = best_min, max = best_max, arg_min = arg_min, arg_max = arg_max,
    arg_min_pool = pools$arg_min_pool, arg_max_pool = pools$arg_max_pool,
    n_failed = n_failed
  )
}

lv_set_extra_candidates <- function(starts, evaluate, check_feasible) {
  flat <- list()
  walk <- function(x) {
    if (is.numeric(x)) {
      flat[[length(flat) + 1L]] <<- unname(x)
    } else if (is.list(x)) {
      for (element in x) walk(element)
    }
  }
  walk(starts)
  pts <- values <- skipped <- list()
  for (b in unique(flat)) {
    feasible <- check_feasible(b)
    if (!isTRUE(feasible$feasible)) {
      skipped[[length(skipped) + 1L]] <- list(
        b = b, reason = "infeasible",
        max_violation = feasible$max_violation
      )
      next
    }
    fit <- evaluate(b, phase = "extra_start")
    if (!log_variance_fit_ok(fit)) {
      skipped[[length(skipped) + 1L]] <- list(b = b, reason = "fit_failure")
      next
    }
    pts[[length(pts) + 1L]] <- b
    values[[length(values) + 1L]] <- unname(fit$coef)
  }
  list(points = pts, values = values, skipped = skipped)
}

lv_set_scan_extremes <- function(v, b, best_min, best_max,
                                 arg_min, arg_max) {
  for (j in seq_along(v)) {
    if (v[j] < best_min[j] ||
      (v[j] == best_min[j] && lv_set_point_precedes(b, arg_min[j, ]))) {
      best_min[j] <- v[j]
      arg_min[j, ] <- b
    }
    if (v[j] > best_max[j] ||
      (v[j] == best_max[j] && lv_set_point_precedes(b, arg_max[j, ]))) {
      best_max[j] <- v[j]
      arg_max[j, ] <- b
    }
  }
  list(min = best_min, max = best_max, arg_min = arg_min, arg_max = arg_max)
}
