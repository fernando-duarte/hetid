# A phase stop describes the numerical search, never the geometry of the entire set
append_growth_trace <- function(phase_history, phase, passes, half, edge, room, max_growth) {
  reason <- if (any(edge & room) && passes >= max_growth) {
    "pass_limit"
  } else if (any(edge & !room)) {
    "search_limit"
  } else {
    "no_boundary_improvement"
  }
  c(phase_history, list(list(
    phase = if (phase == "edge_primary") "coordinates" else "all_objectives",
    passes = passes, half_width = half, edge = edge, stop_reason = reason
  )))
}
