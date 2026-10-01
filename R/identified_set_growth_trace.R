#' Append a Growth-Phase Search Record
#'
#' Records the completed phase's window, boundary flags, pass count, and
#' numerical stopping reason in the search history.
#'
#' Inputs come from the search loop; this helper performs no validation or
#' missing-value removal.
#'
#' @param phase_history List of prior phase records, possibly empty.
#' @param phase Character scalar phase key. The caller uses \code{"edge_primary"}
#'   for the coordinate phase and \code{"edge"} for the all-objectives phase.
#' @param passes Nonnegative integer number of new sweeps in this phase. May be
#'   zero when the second phase reuses the preceding sweep.
#' @param half Numeric vector of window half-widths in slab-frame units, one
#'   per search coordinate.
#' @param edge Logical vector matching \code{half}, flagging coordinates where
#'   a tracked bound improved at the window boundary.
#' @param room Logical vector matching \code{half}, indicating whether doubling
#'   each half-width remains within the search limit.
#' @param max_growth Positive integer maximum number of sweeps per phase.
#' @return The history with one appended list containing \code{phase}
#'   (\code{"coordinates"} for \code{"edge_primary"}, otherwise
#'   \code{"all_objectives"}), \code{passes}, \code{half_width}, \code{edge},
#'   and \code{stop_reason}. The reason is \code{"pass_limit"} when an edge
#'   coordinate can grow but the pass budget is exhausted; otherwise it is
#'   \code{"search_limit"} when an edge coordinate cannot grow, or
#'   \code{"no_boundary_improvement"} when neither condition holds.
#' @noRd
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
