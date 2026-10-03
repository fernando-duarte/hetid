profile_multistart <- function(quadratic, warm, evidence, control) {
  dimension <- ncol(quadratic$A_i[[1L]])
  anchors <- lapply(seq_len(nrow(evidence$feasible_points)), function(i) {
    evidence$feasible_points[i, ]
  })
  accepted <- Filter(
    evidence$check_point,
    profile_dedup_starts(c(warm, anchors), control)
  )
  if (!is.null(evidence$strict_direction)) {
    return(list(points = accepted, evidence = evidence))
  }
  delta <- profile_theta_scale(quadratic)
  axes <- unlist(lapply(seq_len(dimension), function(k) {
    coordinate <- numeric(dimension)
    coordinate[k] <- delta
    list(coordinate, -coordinate)
  }), recursive = FALSE)
  queue <- profile_dedup_starts(
    c(list(numeric(dimension)), axes, warm, anchors),
    control
  )
  search_box <- control$solver_boxes[[1L]]
  solved <- character()
  for (round in seq_len(control$multistart_rounds)) {
    queue <- Filter(function(point) {
      !profile_start_key(point, control) %in% solved
    }, queue)
    if (!length(queue)) break
    solved <- c(solved, vapply(queue, profile_start_key, character(1),
      control = control
    ))
    found <- profile_multistart_round(quadratic, queue, evidence, delta, search_box, control)
    accepted <- profile_dedup_starts(c(accepted, found), control)
    queue <- profile_dedup_starts(found, control)
  }
  # recheck geometry using accepted points as points and directions
  if (is.null(evidence$boundedness) && length(accepted)) {
    evidence <- profile_evidence(quadratic, evidence$objectives,
      points = profile_point_matrix(accepted, dimension),
      directions = profile_point_matrix(accepted, dimension)
    )
  }
  list(points = accepted, evidence = evidence)
}

profile_multistart_round <- function(quadratic, queue, evidence, delta, search_box,
                                     control) {
  dimension <- ncol(quadratic$A_i[[1L]])
  bounds <- profile_scaled_bounds(delta, search_box, dimension)
  found <- list()
  for (k in seq_len(dimension)) {
    for (sign_mult in c(1, -1)) {
      objective <- numeric(dimension)
      objective[k] <- 1
      for (start in queue) {
        result <- solve_quadratic_program(quadratic, start,
          objective = function(theta) sign_mult * sum(objective * theta),
          gradient = function(theta) sign_mult * objective,
          lower = bounds$lower, upper = bounds$upper,
          objective_scale = "variable", control = control, catch_errors = FALSE
        )
        candidate <- profile_checked_candidate(evidence, result$theta, control)
        if (!is.null(candidate)) found[[length(found) + 1L]] <- candidate$theta
      }
    }
  }
  found
}

profile_widen_theta <- function(tab, points) {
  if (!length(points)) {
    return(tab)
  }
  values <- do.call(rbind, points)
  for (k in seq_len(nrow(tab))) {
    if (tab$lower_status[k] == "bounded") {
      tab$set_lower[k] <- min(tab$set_lower[k], values[, k])
    }
    if (tab$upper_status[k] == "bounded") {
      tab$set_upper[k] <- max(tab$set_upper[k], values[, k])
    }
  }
  tab
}

profile_widen_beta1 <- function(tab, beta1r, beta2r, points) {
  if (!length(points)) {
    return(tab)
  }
  for (k in seq_len(nrow(tab))) {
    p <- tab$coef[[k]]
    loading <- beta2r[, p]
    if (all(loading == 0)) next
    values <- vapply(points, function(w) unname(beta1r[[p]] - sum(loading * w)), 0)
    assert_bad_argument_ok(all(is.finite(values)), "Structural map is nonfinite")
    if (tab$lower_status[k] == "bounded") {
      tab$set_lower[k] <- min(tab$set_lower[k], values)
    }
    if (tab$upper_status[k] == "bounded") {
      tab$set_upper[k] <- max(tab$set_upper[k], values)
    }
  }
  tab
}
