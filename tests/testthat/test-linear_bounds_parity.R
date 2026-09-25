test_that("functional coordinates and affine coefficients reproduce the box", {
  for (collinear in c(FALSE, TRUE)) {
    fit <- box_fit(collinear)
    obj <- cbind(diag(ncol(fit$w2)), -fit$beta2r)
    colnames(obj) <- c(colnames(fit$w2), names(fit$beta1r))
    offsets <- c(rep(0, ncol(fit$w2)), unname(fit$beta1r))
    for (tau in c(0.02, 0.05, 0.3)) {
      box <- compute_identified_set_box(fit, tau, n_grid = 7, null_loading_rtol = 0)
      out <- compute_linear_functional_bounds(fit, tau, obj, offsets, n_grid = 7)
      expect_identical(out$bounds, rbind(box$bounds, box$beta1_bounds))
      for (side in c("lower", "upper")) {
        expect_identical(
          unname(out[[paste0("arg_", side)]]),
          unname(rbind(box[[paste0("arg_", side)]], box[[paste0("beta1_arg_", side)]]))
        )
      }
      plain <- identified_set_search(out$center, out$basis, out$quadratic, 3, obj)
      evidence <- identified_set_search(out$center, out$basis, out$quadratic, 3, obj,
        evidence = TRUE
      )
      expect_identical(plain, evidence[names(plain)])
    }
  }
})

test_that("search phases report literal boundary stops with wider allowed windows", {
  fit <- box_fit()
  obj <- cbind(sum = c(1, 1, 1))
  out <- compute_linear_functional_bounds(fit, 0.05, obj, n_grid = 3, search_limit = 8192)
  expect_identical(out$search$phases[[1]]$stop_reason, "no_boundary_improvement")
  expect_identical(out$search$phases[[2]]$passes, 0L)
})
