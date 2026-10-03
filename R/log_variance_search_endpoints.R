lv_set_endpoints <- function(estimator, quadratic, bounds, seed, extra_starts,
                             cold_start_check, tau, evaluate, check_feasible, state,
                             omega, found, diagnostics_of, control) {
  ends <- lv_set_endpoint_state(estimator, found, state)
  extra <- lv_set_endpoint_extras(ends, extra_starts, evaluate, check_feasible)
  polish <- lv_set_refine_endpoints(
    ends, estimator, quadratic, seed, found,
    extra$pool, evaluate, control
  )
  escapes <- lv_set_check_endpoint_box(ends, bounds, control)
  cold <- if (isTRUE(cold_start_check)) {
    lv_set_cold_check(
      estimator$metadata, ends$labels, ends$lower, ends$upper,
      ends$arg_lower, ends$arg_upper, ends$lower_open, ends$upper_open,
      ends$lower_bad, ends$upper_bad, evaluate, control
    )
  } else {
    list(records = list(), lower_bad = ends$lower_bad, upper_bad = ends$upper_bad)
  }
  status <- function(bad, sides) ifelse(bad, "unreliable", ifelse(sides, "unbounded", "bounded"))
  lv_set_result(
    ends$labels, ends$lower, ends$upper,
    status(cold$lower_bad, ends$lower_open), status(cold$upper_bad, ends$upper_open),
    ends$lower_source, ends$upper_source, ends$arg_lower, ends$arg_upper,
    estimator$metadata, tau, quadratic, omega, state$n_failed, state$n_feasible,
    diagnostics_of(list(
      extra_start_skipped = extra$skipped, cold_start = cold$records,
      polish = polish, box_escape = escapes, domain = ends$domain
    ))
  )
}

lv_set_endpoint_state <- function(estimator, found, state) {
  ends <- new.env(parent = emptyenv())
  ends$labels <- state$labels
  n_coef <- length(ends$labels)
  ends$lower_bad <- ends$upper_bad <- rep(FALSE, n_coef)
  # sides the estimator establishes as divergent are infinite and not searched
  ends$lower_open <- ends$upper_open <- rep(FALSE, n_coef)
  if (!is.null(estimator$sides)) {
    sides <- estimator$sides(found, state$precheck)
    assert_bad_argument_ok(is.list(sides), "sides must return a list", "estimator")
    lv_set_side_flags(sides$lower_unbounded, ends$labels, "lower_unbounded")
    lv_set_side_flags(sides$upper_unbounded, ends$labels, "upper_unbounded")
    unresolved <- sides$unresolved_endpoints
    if (!is.null(unresolved)) {
      ends$lower_bad <- paste(ends$labels, "min", sep = ":") %in% unresolved
      ends$upper_bad <- paste(ends$labels, "max", sep = ":") %in% unresolved
    }
    ends$lower_open <- unname(sides$lower_unbounded)
    ends$upper_open <- unname(sides$upper_unbounded)
  }
  ends$lower <- ifelse(ends$lower_open, -Inf, found$min)
  ends$upper <- ifelse(ends$upper_open, Inf, found$max)
  ends$arg_lower <- found$arg_min
  ends$arg_upper <- found$arg_max
  ends$lower_source <- ends$upper_source <- rep("grid", n_coef)

  ends$domain <- if (exists("sides", inherits = FALSE)) sides else NULL
  ends
}

lv_set_endpoint_extras <- function(ends, extra_starts, evaluate,
                                   check_feasible) {
  n_coef <- length(ends$labels)
  extra_pool <- extra_skipped <- list()
  if (!is.null(extra_starts)) {
    extra <- lv_set_extra_candidates(extra_starts, evaluate, check_feasible)
    extra_skipped <- extra$skipped
    extra_pool <- extra$points
    for (i in seq_along(extra$points)) {
      v <- extra$values[[i]]
      for (j in seq_len(n_coef)) {
        if (!ends$lower_open[j] && v[j] < ends$lower[j]) {
          ends$lower[j] <- v[j]
          ends$arg_lower[j, ] <- extra$points[[i]]
          ends$lower_source[j] <- "extra-start"
        }
        if (!ends$upper_open[j] && v[j] > ends$upper[j]) {
          ends$upper[j] <- v[j]
          ends$arg_upper[j, ] <- extra$points[[i]]
          ends$upper_source[j] <- "extra-start"
        }
      }
    }
  }

  list(pool = extra_pool, skipped = extra_skipped)
}

lv_set_check_endpoint_box <- function(ends, bounds, control) {
  n_coef <- length(ends$labels)
  box_escapes <- list()
  for (j in seq_len(n_coef)) {
    for (side in c("min", "max")) {
      if (if (side == "min") ends$lower_open[j] else ends$upper_open[j]) next
      excess <- lv_set_box_escape(
        if (side == "min") ends$arg_lower[j, ] else ends$arg_upper[j, ],
        bounds
      )
      if (is.na(excess) || excess <= control$search$box_escape_rtol) next
      if (side == "min") ends$lower_bad[j] <- TRUE else ends$upper_bad[j] <- TRUE
      box_escapes[[length(box_escapes) + 1L]] <- list(
        coef = ends$labels[j], side = side,
        excess = excess
      )
    }
  }
  box_escapes
}
