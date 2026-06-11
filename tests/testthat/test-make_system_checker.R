test_that("system checker evaluates every constraint at theta", {
  sys <- make_general_test_system()
  lambda <- list(
    matrix(rnorm(8), 4, 2), matrix(rnorm(4), 4, 1), matrix(rnorm(4), 4, 1)
  )
  qs <- build_general_quadratic_system(lambda, 0.2, sys$moments)
  check_all <- make_system_checker(qs$quadratic)

  theta <- rnorm(3)
  values <- check_all(theta)
  expect_identical(names(values), qs$labels$name)
  manual <- vapply(seq_len(nrow(qs$labels)), function(k) {
    as.numeric(
      t(theta) %*% qs$quadratic$A_i[[k]] %*% theta +
        sum(qs$quadratic$b_i[[k]] * theta) + qs$quadratic$c_i[k]
    )
  }, numeric(1))
  expect_equal(unname(values), manual)
})

test_that("system checker agrees with the single-constraint factory", {
  sys <- make_general_test_system()
  gamma <- matrix(rnorm(12), 4, 3)
  qs <- build_quadratic_system(gamma, c(0, 0.1, 0.2), sys$moments)
  check_all <- make_system_checker(qs$quadratic)
  single <- make_constraint_checker(
    qs$quadratic$A_i[[2]], qs$quadratic$b_i[[2]], qs$quadratic$c_i[2]
  )
  theta <- rnorm(3)
  expect_equal(
    unname(check_all(theta)[2]), as.numeric(single(theta))
  )
})

test_that("malformed quadratic lists are rejected", {
  expect_error(
    make_system_checker(list(A_i = list(diag(2)), b_i = list())),
    class = "hetid_error_bad_argument"
  )
})
