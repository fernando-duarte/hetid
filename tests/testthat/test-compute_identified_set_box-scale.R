test_that("common response units preserve theta bounds and rescale beta1", {
  dat <- simulate_box_dgp()
  reference <- compute_identified_set_box(
    compute_tau0_system(dat$y1, dat$y2, dat$x, dat$z),
    tau = 0.05, n_grid = 11L
  )
  for (s in c(0.001, 0.01, 0.1, 100)) {
    box <- compute_identified_set_box(
      compute_tau0_system(s * dat$y1, s * dat$y2, dat$x, dat$z),
      tau = 0.05, n_grid = 11L
    )
    # Every feasible point satisfies the sum of the inequalities. Positive
    # definite curvature of that sum independently proves the set is bounded
    eig <- eigen(Reduce(`+`, box$quadratic$A_i), symmetric = TRUE)$values
    expect_gt(min(eig), 0)
    expect_true(all(is.finite(unlist(box$bounds[c("lower", "upper")]))))
    expect_equal(box$bounds, reference$bounds, tolerance = 1e-10)
    expect_equal(box$beta1_bounds$lower / s, reference$beta1_bounds$lower,
      tolerance = 1e-10
    )
    expect_equal(box$beta1_bounds$upper / s, reference$beta1_bounds$upper,
      tolerance = 1e-10
    )
    # Boundary arithmetic has the units of the quadratic; check the witnesses
    # against the original-unit system rather than an absolute scaled cutoff
    checker <- make_system_checker(reference$quadratic)
    witnesses <- rbind(
      box$arg_lower, box$arg_upper,
      box$beta1_arg_lower, box$beta1_arg_upper
    )
    expect_lte(max(apply(witnesses, 1L, function(w) max(checker(w)))), 1e-10)
  }
})
