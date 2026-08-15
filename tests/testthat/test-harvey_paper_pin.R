# Paper-equivalence pin for the Harvey port. The coefficient vector, the
# iteration count, and all five covariance matrices come from the package chain
# and were verified equal (tolerance 1e-10, on every field the two sides share:
# coef, warm_start, objective, score_norm, convergence_code, n_zero_response,
# rank_x_pos, rcond_info, n_halvings, and the start ladder) to the paper
# pipeline's logvar_harvey_fit_response / logvar_harvey_fit / logvar_harvey_vcov
# against scripts-paper at HEAD eaa9ad3bff80978b9b26b6e73079af44fd35e0b3 on
# 2026-08-15, by the procedure in docs/verification/harvey_port_equivalence.R
# (local, git-ignored; re-run it to re-verify). Full matrices, dimnames
# included: a diagonal-only pin would let off-diagonal drift pass. The paper
# files that own the fit and the SEs, with sha256:
#   engine/contracts.R:
#     c3cd07a4b9ca4b33c55da2038ac5bc673a73e101d37158a069c905ba3f57efbd
#   estimators/controls.R:
#     cc336681b4f2a62257020ef236d2ccdd750d24ade214a72d5faf52aaab2adc55
#   harvey/likelihood.R:
#     63b88226b59c4eca08396bff7150ec8250ed4450b98793d5345205b3f66c3bbf
#   harvey/solver_primitives.R:
#     1b1d9f9281268e0a941e54bd112093636208d0796f331ac837793cbc15beeeb9
#   harvey/solver_result.R:
#     986dca8b672ae76d503756b1efa054b866b4b878933effaa2fa88d12349da9e5
#   harvey/solver_acceptance.R:
#     643ae7db824250020e23643a7b2d8420e329307e60ef798d8df57b3f1fe11af3
#   harvey/solver.R:
#     51ce2cfded0af7a779691fbd2228bab762869b8ea879ec217f6af17562ade022
#   inference/standard_error_estimators.R:
#     d1c71657f788ed9fc6cb1f7fe50e308fb1d427450c3cbe90de857f77917d2ffb
#   harvey/standard_errors.R:
#     c4e9ecafa50c7f98378e8e414a064c11f1ccf822cb17091e7587064c68e25a25
# The test itself never sources the paper pipeline.
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
