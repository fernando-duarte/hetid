test_that("search and tau defaults preserve the frozen donor values", {
  expect_identical(QUADRATIC_PROFILE_CONTROL, list(
    constraint_scale_floor_rtol = 1e-12, symmetry_rtol = 1e-8,
    solver_boxes = c(1e6, 1e9, 1e10), solver_xtol_rel = 1e-8,
    solver_maxeval = 1000L, feasibility_tolerance = 1e-4,
    admission_tolerance = 1e-10, candidate_correction_rtol = 1e-6,
    bound_edge_rtol = 0.99, bound_stability_rtol = 1e-3,
    multistart_rounds = 4L, multistart_dedup_digits = 6L
  ))
  expect_identical(MEAN_TAU_CONTROL, list(
    cap = 0.99, sweep_step = 0.005, bisection_iterations = 40L
  ))
})
