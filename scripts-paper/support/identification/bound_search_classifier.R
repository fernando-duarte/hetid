# Boundedness and tails come from geometry; boxes only guide finite approximation.
.profile_invalid_bound <- function() {
  list(bound = NA_real_, bounded = FALSE, valid = FALSE)
}

.profile_unbounded_bound <- function(value) {
  list(bound = value, bounded = FALSE, valid = TRUE)
}

.classify_profile_search <- function(
  objective, direction, boxes, solve_box, delta, evidence, evidence_index,
  candidate_is_endpoint,
  edge_rtol = PAPER_QUADRATIC_CONTROL$bound_edge_rtol,
  stability_rtol = PAPER_QUADRATIC_CONTROL$bound_stability_rtol
) {
  state <- profile_evidence_state(evidence, evidence_index, direction)
  if (identical(state, "unbounded")) {
    return(.profile_unbounded_bound(if (direction == "min") -Inf else Inf))
  }
  if (!identical(state, "bounded")) {
    return(.profile_invalid_bound())
  }
  if (all(objective == 0)) {
    return(list(bound = 0, bounded = TRUE, valid = TRUE))
  }
  normalized <- profile_objective_direction(objective)
  if (is.null(normalized)) {
    return(.profile_invalid_bound())
  }
  previous <- NULL
  for (box in boxes) {
    result <- solve_box(box)
    if (!.solve_finite(result)) next
    candidate <- profile_checked_candidate(evidence, delta * result$phi)
    if (is.null(candidate)) next
    if (!candidate_is_endpoint(candidate$theta)) next
    value <- sum(objective * candidate$theta)
    normalized_value <- sum(normalized * candidate$theta)
    if (!is.finite(value) || !is.finite(normalized_value)) next
    interior <- all(abs(result$phi) < edge_rtol * box)
    stable <- !is.null(previous) &&
      abs(normalized_value - previous) <= stability_rtol * max(1, abs(normalized_value))
    if (interior || stable) {
      return(list(
        bound = value, bounded = TRUE, valid = TRUE,
        theta = candidate$theta, contraction = candidate$contraction,
        movement = candidate$movement
      ))
    }
    previous <- normalized_value
  }
  .profile_invalid_bound()
}
