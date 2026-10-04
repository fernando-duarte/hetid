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
  CONSTRAINT_SCALE_FLOOR_RTOL = 1e-12,
  SYMMETRY_RTOL = 1e-8,
  SOLVER_BOXES = c(1e6, 1e9, 1e10),
  SOLVER_XTOL_REL = 1e-8,
  SOLVER_MAXEVAL = 1000L,
  FEASIBILITY_TOLERANCE = 1e-4,
  ADMISSION_TOLERANCE = 1e-10,
  CANDIDATE_CORRECTION_RTOL = 1e-6,
  BOUND_EDGE_RTOL = 0.99,
  BOUND_STABILITY_RTOL = 1e-3,
  MULTISTART_ROUNDS = 4L,
  MULTISTART_DEDUP_DIGITS = 6L
)
