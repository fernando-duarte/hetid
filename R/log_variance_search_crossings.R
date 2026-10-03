lv_set_logols_control <- function() {
  list(
    estimator_version = "logols-v1", cold_start_rtol = 1e-8, crossing_range_rtol = 1e-8,
    scan_chunk_size = 5000L,
    # the whole lattice is scanned, so its largest size is bounded before it is built
    full_grid_safety_cap = 1e6
  )
}

lv_set_box_clear <- function(lower, upper, w1, w2, slack = 0) {
  vapply(seq_along(w1), function(i) {
    bound <- lv_log_sum(lapply(seq_along(lower), function(j) {
      lv_log_op(rep(w2[i, j], 2L), c(lower[j], upper[j]), "*")
    }))
    w1[i] < bound[1L] || w1[i] > bound[2L]
  }, logical(1))
}

lv_set_crossing_census <- function(quadratic, lower, upper, w1, w2, control,
                                   sets = lv_set_solver_control(), groups = NULL) {
  constant <- rowSums(w2 != 0) == 0L
  zero <- which(constant & w1 == 0)
  out <- list(cross = integer(0), unresolved = integer(0), zero_rows = zero)
  if (length(zero)) {
    return(out)
  }
  rows <- which(!constant & !lv_set_box_clear(lower, upper, w1, w2))
  if (!length(rows)) {
    return(out)
  }
  if (is.null(groups)) {
    groups <- list(rows = lapply(seq_along(w1), identity), ambiguous = integer(0))
  }
  active <- Filter(function(group) any(group %in% rows), groups$rows)
  dimension <- ncol(w2)
  coordinate_forms <- vector("list", dimension)
  forms_for <- function(j) {
    if (is.null(coordinate_forms[[j]])) {
      coordinate_forms[[j]] <<- lv_log_line_forms(
        quadratic, w1, w2, rep(0, dimension), diag(dimension)[, j]
      )
    }
    coordinate_forms[[j]]
  }
  pending <- list()
  for (group in active) {
    if (any(group %in% groups$ambiguous)) {
      out$unresolved <- union(out$unresolved, group)
      next
    }
    witnessed <- FALSE
    for (j in seq_len(dimension)) {
      if (lv_set_line_approach(
        quadratic, w1, w2, group,
        rep(0, dimension), diag(dimension)[, j], function() forms_for(j)
      )) {
        witnessed <- TRUE
        break
      }
    }
    if (witnessed) {
      out$cross <- union(out$cross, group)
    } else {
      pending[[length(pending) + 1L]] <- group
    }
  }
  if (length(pending)) {
    out <- lv_log_pending_census(out, pending, quadratic, w1, w2, sets)
  }
  out$cross <- sort(out$cross)
  out$unresolved <- sort(out$unresolved)
  out
}

lv_log_pending_census <- function(out, pending, quadratic, w1, w2, sets) {
  representatives <- vapply(pending, `[`, integer(1), 1L)
  vectors <- t(w2[representatives, , drop = FALSE])
  evidence <- profile_evidence(quadratic, vectors)
  outer_limits <- evidence$outer_bounds(vectors, refine = TRUE)
  for (j in seq_along(pending)) {
    target_row <- representatives[j]
    clear <- (!is.na(outer_limits$lower[j]) && w1[target_row] < outer_limits$lower[j]) ||
      (!is.na(outer_limits$upper[j]) && w1[target_row] > outer_limits$upper[j])
    if (clear) next
    low <- profile_linear_bound(quadratic, vectors[, j], "min", evidence, j, sets)
    high <- profile_linear_bound(quadratic, vectors[, j], "max", evidence, j, sets)
    witnessed <- !is.null(low$theta) && !is.null(high$theta) &&
      lv_set_line_approach(quadratic, w1, w2, pending[[j]], low$theta, high$theta - low$theta)
    if (witnessed) {
      out$cross <- union(out$cross, pending[[j]])
    } else {
      out$unresolved <- union(out$unresolved, pending[[j]])
    }
  }
  out
}
