#' Controls for Quadratic Profile Searches
#'
#' Numerical controls of the scaled SLSQP and coefficient refinement procedure.
#' Solver box growth guides finite search; it does not establish boundedness.
#' The defaults preserve the donor calculation's arithmetic and search schedule.
#'
#' @format A named list. Constraint scaling uses a median-relative floor of
#'   1e-12; symmetry tolerance is 1e-8. Solver boxes are c(1e6, 1e9, 1e10),
#'   xtol_rel is 1e-8, and maxeval is 1000. Endpoint activity tolerance is 1e-4;
#'   grid admission tolerance is 1e-10. Candidate repair may move at most 1e-6
#'   in relative sup norm. Edge and stability thresholds are 0.99 and 1e-3.
#'   Multistart uses four rounds and six significant digits for duplicate keys.
#' @export
QUADRATIC_PROFILE_CONTROL <- list(
  constraint_scale_floor_rtol = 1e-12,
  symmetry_rtol = 1e-8,
  solver_boxes = c(1e6, 1e9, 1e10),
  solver_xtol_rel = 1e-8,
  solver_maxeval = 1000L,
  feasibility_tolerance = 1e-4,
  admission_tolerance = 1e-10,
  candidate_correction_rtol = 1e-6,
  bound_edge_rtol = 0.99,
  bound_stability_rtol = 1e-3,
  multistart_rounds = 4L,
  multistart_dedup_digits = 6L
)
