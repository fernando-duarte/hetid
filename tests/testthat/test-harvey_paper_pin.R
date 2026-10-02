# pinned to the paper pipeline's Harvey fit and vcov (equal to 1e-10, 2026-08-15); full
# matrices so off-diagonal drift fails; the test never sources the paper pipeline
test_that("pinned paper-equivalence fixture: harvey coef and vcov at the default seed", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x, estimator = "harvey")
  expect_equal(
    fit$coef,
    c(
      `(Intercept)` = -0.64035513184111792, v1 = 0.5089790023756976,
      v2 = -0.44676739018802147
    ),
    tolerance = 1e-8
  )
  expect_identical(fit$convergence_code, 5L)

  labels <- c("(Intercept)", "v1", "v2")
  pin <- function(values) matrix(values, 3L, 3L, dimnames = list(labels, labels))
  expected <- list(
    expected = pin(c(
      0.0067093289419982594, -0.00054321789892893013, 0.00010722400976845304,
      -0.00054321789892893013, 0.0069247586697068052, -0.0011379716381053524,
      0.00010722400976845304, -0.0011379716381053524, 0.0067476078543175465
    )),
    observed = pin(c(
      0.0067052299484383383, -0.00049044941363281388, 0.00011333975384313437,
      -0.00049044941363281388, 0.0062459139793663437, -0.0012032630217381113,
      0.00011333975384313437, -0.0012032630217381113, 0.0071215049976653426
    )),
    opg = pin(c(
      0.0065170857917649528, -0.0011930748709537275, 0.00097829067893791906,
      -0.0011930748709537275, 0.0065957700333187479, -0.0013775514721547108,
      0.00097829067893791906, -0.0013775514721547108, 0.0071917961826729543
    )),
    robust = pin(c(
      0.0070675072042435463, 0.00020387717724384527, -0.00078166036090889662,
      0.00020387717724384537, 0.005979593050718917, -0.0011225305899685712,
      -0.00078166036090889651, -0.0011225305899685712, 0.0071666239893553073
    )),
    hac = pin(c(
      0.0077692842310512133, -5.6195470375549939e-05, -0.00037000745933808395,
      -5.6195470375549789e-05, 0.0059903166826490795, -0.00033906176563395504,
      -0.000370007459338084, -0.00033906176563395563, 0.0071664917334282544
    ))
  )

  vc <- compute_log_variance_vcov(fit, hac_lags = 4L)
  expect_identical(names(vc), names(expected))
  for (variant in names(expected)) {
    expect_equal(vc[[variant]], expected[[variant]], tolerance = 1e-8)
  }
})
