lv_set_polish_side <- function(ends, j, side, found, seed, extra_pool,
                               quadratic, guard_scale, objective, budget_hit, control) {
  starts <- lv_set_polish_starts(ends, j, side, found, seed, extra_pool)
  if (!is.null(objective$admit)) starts <- Filter(objective$admit, starts)
  accepted <- FALSE
  for (start in starts) {
    polished <- lv_set_polish(
      quadratic, side, start, guard_scale,
      objective$fn, objective$gr, control
    )
    if (!is.null(budget_hit$condition)) stop(budget_hit$condition)
    if (polished$suspect) {
      if (side == "min") ends$lower_bad[j] <- TRUE else ends$upper_bad[j] <- TRUE
    }
    if (is.null(polished$bound)) next
    accepted <- TRUE
    if (side == "min" && polished$bound < ends$lower[j]) {
      ends$lower[j] <- polished$bound
      ends$arg_lower[j, ] <- polished$par
      ends$lower_source[j] <- "polish"
    }
    if (side == "max" && polished$bound > ends$upper[j]) {
      ends$upper[j] <- polished$bound
      ends$arg_upper[j, ] <- polished$par
      ends$upper_source[j] <- "polish"
    }
  }
  if (!accepted) {
    if (side == "min") ends$lower_bad[j] <- TRUE else ends$upper_bad[j] <- TRUE
  }
  list(
    coef = ends$labels[j], side = side,
    n_trials = length(starts), accepted = accepted
  )
}

lv_set_refine_endpoints <- function(ends, estimator, quadratic, seed, found,
                                    extra_pool, evaluate, control) {
  budget_hit <- new.env(parent = emptyenv())
  budget_hit$condition <- NULL
  records <- list()
  for (j in seq_along(ends$labels)) {
    scanned <- c(found$min[j], found$max[j])
    guard_scale <- max(1, abs(scanned[is.finite(scanned)]))
    objective <- lv_set_objective(estimator, j, evaluate, budget_hit)
    for (side in c("min", "max")) {
      is_open <- if (side == "min") ends$lower_open[j] else ends$upper_open[j]
      if (is_open) next
      records[[length(records) + 1L]] <- lv_set_polish_side(
        ends, j, side, found,
        seed, extra_pool, quadratic, guard_scale, objective, budget_hit, control
      )
    }
  }
  records
}

lv_set_polish_starts <- function(ends, j, side, found, seed, extra_pool) {
  seeded <- !is.null(seed) && !anyNA(seed)
  starts <- list(if (side == "min") found$arg_min[j, ] else found$arg_max[j, ])
  pool <- if (side == "min") found$arg_min_pool else found$arg_max_pool
  if (!is.null(pool) && length(pool) >= j) starts <- c(starts, pool[[j]])
  if (seeded) starts <- c(starts, list(seed))
  starts <- c(starts, extra_pool)
  starts
}
