profile_status_worst <- function(status) {
  priority <- c("bounded", "unbounded", "unreliable")
  assert_bad_argument_ok(is.character(status) && length(status) > 0L &&
    !anyNA(status) && all(status %in% priority), "Invalid endpoint status", arg = "status")
  priority[[max(match(status, priority))]]
}

profile_status_from_flags <- function(bounded, valid) {
  assert_bad_argument_ok(length(bounded) == length(valid) &&
    !anyNA(bounded) && !anyNA(valid), "Invalid endpoint flags")
  ifelse(!valid, "unreliable", ifelse(bounded, "bounded", "unbounded"))
}

profile_evidence <- function(quadratic, objectives, points = NULL, directions = NULL) {
  out <- compute_quadratic_set_evidence(quadratic, objectives,
    points = points, directions = directions
  )
  out$objectives <- objectives
  out
}

profile_point_matrix <- function(points, dimension) {
  points <- Filter(function(point) {
    is.numeric(point) && length(point) == dimension && all(is.finite(point))
  }, points)
  if (!length(points)) {
    return(matrix(numeric(), 0L, dimension))
  }
  do.call(rbind, points)
}

profile_checked_candidate <- function(evidence, theta, control, normalized = NULL) {
  if (evidence$check_point(theta)) {
    return(list(theta = theta))
  }
  if (any(!is.finite(theta)) || !nrow(evidence$feasible_points)) {
    return(NULL)
  }
  point_scale <- max(1, abs(theta))
  distance <- apply(evidence$feasible_points, 1L, function(point) {
    max(abs(point / point_scale - theta / point_scale))
  })
  for (fraction in 2^seq(-40, 0)) {
    for (i in order(distance)) {
      anchor <- evidence$feasible_points[i, ]
      candidate <- (1 - fraction) * theta + fraction * anchor
      movement <- max(abs(candidate / point_scale - theta / point_scale))
      if (movement <= control$CANDIDATE_CORRECTION_RTOL && evidence$check_point(candidate)) {
        return(list(theta = candidate))
      }
    }
  }
  profile_segment_candidate(evidence, theta, normalized)
}

profile_objective_direction <- function(objective) {
  magnitude <- max(abs(objective))
  if (magnitude == 0) {
    return(objective)
  }
  normalized <- objective / magnitude
  normalized <- normalized / sqrt(sum(normalized^2))
  if (any(objective != 0 & normalized == 0)) {
    return(NULL)
  }
  normalized
}

profile_start_key <- function(point, control) {
  paste(signif(point, control$MULTISTART_DEDUP_DIGITS), collapse = "|")
}

profile_dedup_starts <- function(points, control) {
  points <- Filter(function(point) {
    !is.null(point) && length(point) && all(is.finite(point))
  }, points)
  points[!duplicated(vapply(points, profile_start_key, character(1),
    control = control
  ))]
}
