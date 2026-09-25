test_that("paired endpoint root algebra retains independent oracles", {
  set.seed(20260730L)

  et_alpha <- 0.1
  et_tol <- 1e-4
  et_stability <- 0.85
  et_b <- 100L
  et_min_reps <- 50L
  et_pool <- rep(TRUE, et_b)
  et_check <- function(...) expect_true(all(vapply(list(...), isTRUE, logical(1))))
  # the conservative rank, and both per-draw roots, written out from their
  # definitions rather than borrowed from the module, so the ordering identity
  # below is tested against the spec and not against itself
  et_rank <- function(n, alpha) min(n, ceiling((n + 1) * (1 - alpha)))
  et_root_s <- function(z_l, z_u) pmax(0, z_l, z_u)
  et_root_p <- function(z_l, z_u, d_l, d_u, lambda) {
    pmax(0, z_l - lambda * d_l, z_u - (1 - lambda) * d_u)
  }
  et_quant_p <- function(z_l, z_u, d_l, d_u, lambda) {
    bootstrap_root_critical(et_root_p(z_l, z_u, d_l, d_u, lambda), et_alpha)
  }

  # conservative rank
  et_vals <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
  et_check(
    bootstrap_root_critical(as.numeric(1:20), 0.10) == 19,
    bootstrap_root_critical(as.numeric(1:20), 0.01) == 20,
    bootstrap_root_critical(et_vals, 0.5) == sort(et_vals)[et_rank(10L, 0.5)],
    identical(bootstrap_root_critical(numeric(0), et_alpha), NA_real_)
  )
  for (et_n in c(3L, 7L, 41L)) {
    et_x <- stats::rnorm(et_n)
    et_check(bootstrap_root_critical(et_x, et_alpha) == sort(et_x)[et_rank(et_n, et_alpha)])
  }

  # drawwise domination
  for (et_iter in seq_len(20L)) {
    et_z_l <- stats::rnorm(50L)
    et_z_u <- stats::rnorm(50L)
    et_d_l <- stats::runif(1L, 0, 4)
    et_d_u <- stats::runif(1L, 0, 4)
    for (et_lambda in c(0, 0.13, 0.5, 0.79, 1)) {
      et_check(all(
        et_root_p(et_z_l, et_z_u, et_d_l, et_d_u, et_lambda) <=
          et_root_s(et_z_l, et_z_u)
      ))
    }
    et_check(isTRUE(all.equal(
      bootstrap_containment_critical(rep(TRUE, 50L), et_alpha, et_z_l, et_z_u),
      bootstrap_root_critical(et_root_s(et_z_l, et_z_u), et_alpha)
    )))
  }

  # ordering, non-tautologically
  for (et_iter in seq_len(30L)) {
    et_z_l <- stats::rnorm(60L, -0.2)
    et_z_u <- stats::rnorm(60L, -0.2)
    et_d_l <- stats::runif(1L, 0.2, 3)
    et_d_u <- stats::runif(1L, 0.2, 3)
    et_cs <- bootstrap_containment_critical(rep(TRUE, 60L), et_alpha, et_z_l, et_z_u)
    et_p <- bootstrap_pointwise_critical(
      et_z_l, et_z_u, rep(TRUE, 60L), et_d_l, et_d_u, et_alpha, et_tol, et_cs
    )
    et_check(
      et_p$c_p_lower <= et_cs + 1e-12,
      isTRUE(all.equal(
        et_p$c_p_lower,
        et_quant_p(et_z_l, et_z_u, et_d_l, et_d_u, et_p$best_lambda)
      )),
      et_p$c_p_upper - et_p$c_p_lower >= -1e-12,
      et_p$c_p_upper - et_p$c_p_lower <= et_tol
    )
  }

  # degenerate width
  et_z_l <- stats::rnorm(40L)
  et_z_u <- stats::rnorm(40L)
  et_cs <- bootstrap_containment_critical(rep(TRUE, 40L), et_alpha, et_z_l, et_z_u)
  et_flat <- bootstrap_pointwise_critical(
    et_z_l, et_z_u, rep(TRUE, 40L), 0, 0, et_alpha, et_tol, et_cs
  )
  et_check(
    identical(et_flat$evals, 0L), et_flat$c_p_lower == et_cs,
    et_flat$c_p_upper == et_cs, identical(et_flat$interior, FALSE)
  )

  # the adversarial interior optimum
  # 84 draws with no inward deviation, 8 that only threaten the lower endpoint and
  # 8 that only threaten the upper one, with unit credits on both sides. The
  # conservative order statistic is exactly zero on the coarse grid and 1/8 at
  # lambda equal to 1/8, so only a certified search over the continuum finds it.
  et_adv_l <- c(rep(0, 84L), rep(0.25, 8L), rep(0, 8L))
  et_adv_u <- c(rep(0, 84L), rep(0, 8L), rep(1, 8L))
  et_adv_cs <- bootstrap_containment_critical(et_pool, et_alpha, et_adv_l, et_adv_u)
  et_adv <- bootstrap_pointwise_critical(
    et_adv_l, et_adv_u, et_pool, 1, 1, et_alpha, et_tol, et_adv_cs
  )
  et_coarse <- vapply(
    c(0, 0.25, 0.5, 0.75, 1),
    function(lambda) et_quant_p(et_adv_l, et_adv_u, 1, 1, lambda),
    numeric(1)
  )
  et_check(
    all(et_coarse == 0),
    isTRUE(all.equal(et_quant_p(et_adv_l, et_adv_u, 1, 1, 0.125), 0.125)),
    abs(et_adv$c_p_lower - 0.125) <= et_tol,
    isTRUE(all.equal(et_adv$best_lambda, 0.125)),
    et_adv$c_p_lower > max(et_coarse),
    isTRUE(et_adv$interior), identical(et_adv$evals, 6L),
    et_adv$c_p_upper - et_adv$c_p_lower <= et_tol,
    isTRUE(all.equal(et_adv_cs, 0.25))
  )

  # interior optimum with asymmetric credits
  # the same shape with credits of 1 and 5, so the Lipschitz constant has to be
  # the larger of the two: the peak rides the steep upper-side face
  et_asym_u <- c(rep(0, 84L), rep(0, 8L), rep(5.1, 8L))
  et_asym_cs <- bootstrap_containment_critical(et_pool, et_alpha, et_adv_l, et_asym_u)
  et_asym <- bootstrap_pointwise_critical(
    et_adv_l, et_asym_u, et_pool, 1, 5, et_alpha, et_tol, et_asym_cs
  )
  et_dense <- seq(0, 1, length.out = 2001L)
  et_dense_q <- vapply(
    et_dense,
    function(lambda) et_quant_p(et_adv_l, et_asym_u, 1, 5, lambda),
    numeric(1)
  )
  et_check(
    max(et_dense_q) > max(et_dense_q[c(1L, 2001L)]),
    et_asym$c_p_upper >= max(et_dense_q) - 1e-12,
    et_asym$c_p_lower <= max(et_dense_q) + 1e-12,
    max(et_dense_q) - et_asym$c_p_lower <= et_tol,
    et_asym$c_p_upper - et_asym$c_p_lower >= -1e-12,
    et_asym$c_p_upper - et_asym$c_p_lower <= et_tol,
    isTRUE(et_asym$interior)
  )

  # stopping-test economy
  # a flat credited quantile with a steep slope: the reported bound is capped by
  # Target S, which is itself zero here, so the search must stop at once instead
  # of refining to a width of twice the tolerance over the Lipschitz constant
  et_cheap <- bootstrap_pointwise_critical(
    rep(-3, et_b), rep(-3, et_b), et_pool, 10, 10, et_alpha, et_tol,
    bootstrap_containment_critical(et_pool, et_alpha, rep(-3, et_b), rep(-3, et_b))
  )
  et_check(
    et_cheap$evals <= 10L, et_cheap$c_p_lower == 0, et_cheap$c_p_upper == 0
  )

  expect_error(bootstrap_root_critical(c(1, Inf), 0.1), class = "hetid_error")
})
