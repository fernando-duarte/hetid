# Geometry is evaluated once for the objectives sharing a quadratic system.
paper_profile_evidence <- function(quadratic, objectives, points = NULL,
                                   directions = NULL) {
  out <- hetid::compute_quadratic_set_evidence(
    quadratic, objectives,
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

# An optimizer's boundary tolerance is not a membership witness. If necessary,
# contract toward a checked interior point, retaining the adjustment diagnostic.
profile_checked_candidate <- function(evidence, theta,
                                      max_correction =
                                        PAPER_QUADRATIC_CONTROL$candidate_correction_rtol) {
  if (evidence$check_point(theta)) {
    return(list(theta = theta, contraction = 0, movement = 0))
  }
  if (any(!is.finite(theta)) || !nrow(evidence$feasible_points)) {
    return(NULL)
  }
  scale <- max(1, abs(theta))
  distance <- apply(evidence$feasible_points, 1L, function(point) {
    max(abs(point / scale - theta / scale))
  })
  # The displacement cap controls numerical distortion. The interpolation
  # fraction alone does not: thin sets can have an anchor very near an endpoint.
  fractions <- 2^seq(-40, 0)
  for (fraction in fractions) {
    for (i in order(distance)) {
      anchor <- evidence$feasible_points[i, ]
      candidate <- (1 - fraction) * theta + fraction * anchor
      movement <- max(abs(candidate / scale - theta / scale))
      if (movement <= max_correction && evidence$check_point(candidate)) {
        return(list(theta = candidate, contraction = fraction, movement = movement))
      }
    }
  }
  NULL
}

profile_evidence_state <- function(evidence, index, direction) {
  side <- if (direction == "min") "lower_state" else "upper_state"
  evidence$summary[[side]][index]
}

# Retain the size of every finite candidate adjustment for offline auditing.
profile_correction_record <- function(result, objective, side, source = "profile") {
  data.frame(
    objective = objective, side = side, source = source,
    contraction = if (is.null(result$contraction)) NA_real_ else result$contraction,
    movement = if (is.null(result$movement)) NA_real_ else result$movement
  )
}
