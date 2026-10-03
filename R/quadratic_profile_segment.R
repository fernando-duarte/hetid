# Fallback repair for a solver point that stops just outside a thin set, where the
# capped move toward a verified point is too short. Along the segment to each
# verified anchor, nearest first, it brackets the first checked point within the
# segment movement cap and the objective-change budget, then bisects back toward
# the solver point. Only points that pass check_point are returned.
profile_segment_candidate <- function(evidence, theta, normalized = NULL) {
  anchors <- evidence$feasible_points
  if (!nrow(anchors)) {
    return(NULL)
  }
  point_scale <- max(1, abs(theta))
  distance <- apply(anchors, 1L, function(anchor) {
    max(abs(anchor / point_scale - theta / point_scale))
  })
  budget <- if (is.null(normalized)) {
    Inf
  } else {
    HETID_CONSTANTS$PROFILE_SEGMENT_OBJECTIVE_RTOL * max(1, abs(sum(normalized * theta)))
  }
  for (i in order(distance)) {
    anchor <- anchors[i, ]
    gain <- if (is.null(normalized)) 0 else abs(sum(normalized * (anchor - theta)))
    limit <- min(1, HETID_CONSTANTS$PROFILE_SEGMENT_RTOL / distance[i])
    if (gain > 0) limit <- min(limit, budget / gain)
    if (!is.finite(limit) || limit <= 0) next
    point <- function(lambda) (1 - lambda) * theta + lambda * anchor
    lambda <- profile_segment_lambda(evidence$check_point, point, limit)
    if (!is.na(lambda)) {
      return(list(theta = point(lambda)))
    }
  }
  NULL
}

# Smallest checked step found on the segment: a dyadic scan up to limit brackets
# the first checked point, then bisection shrinks the step while points pass.
profile_segment_lambda <- function(check_point, point, limit) {
  maxit <- HETID_CONSTANTS$PROFILE_SEGMENT_MAXIT
  low <- 0
  high <- NA_real_
  for (lambda in 2^seq(-maxit, 0) * limit) {
    if (check_point(point(lambda))) {
      high <- lambda
      break
    }
    low <- lambda
  }
  if (is.na(high)) {
    return(NA_real_)
  }
  for (iteration in seq_len(maxit)) {
    mid <- low + (high - low) / 2
    if (mid == low || mid == high) break
    if (check_point(point(mid))) high <- mid else low <- mid
  }
  high
}
