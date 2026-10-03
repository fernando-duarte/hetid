lv_set_lattice_limit <- function(estimator, control) {
  limit <- control$sets$grid_points_limit
  if (!is.null(estimator$full_grid_safety_cap)) {
    lv_set_positive(estimator$full_grid_safety_cap, "full_grid_safety_cap", TRUE)
    limit <- min(limit, estimator$full_grid_safety_cap)
  }
  limit
}

lv_set_check_lattice <- function(n_axis, dimension, limit) {
  lv_set_positive(n_axis, "grid_n", TRUE)
  lv_set_positive(limit, "raw lattice limit", TRUE)
  size <- n_axis^dimension
  assert_bad_argument_ok(
    is.finite(size) && size <= limit,
    paste0("raw lattice exceeds its ", format(limit), " point limit before allocation"),
    "grid_n"
  )
  invisible(TRUE)
}
