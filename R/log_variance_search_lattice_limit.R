lv_set_lattice_limit <- function(estimator, control) {
  limit <- control$sets$GRID_POINTS_LIMIT
  if (!is.null(estimator$FULL_GRID_SAFETY_CAP)) {
    lv_set_positive(estimator$FULL_GRID_SAFETY_CAP, "FULL_GRID_SAFETY_CAP", TRUE)
    limit <- min(limit, estimator$FULL_GRID_SAFETY_CAP)
  }
  limit
}

lv_set_check_lattice <- function(n_axis, dimension, limit) {
  lv_set_positive(n_axis, "GRID_N", TRUE)
  lv_set_positive(limit, "raw lattice limit", TRUE)
  size <- n_axis^dimension
  assert_bad_argument_ok(
    is.finite(size) && size <= limit,
    paste0("raw lattice exceeds its ", format(limit), " point limit before allocation"),
    "GRID_N"
  )
  invisible(TRUE)
}
