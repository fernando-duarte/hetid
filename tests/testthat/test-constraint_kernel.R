fixture_path <- test_path("fixtures", "quadratic_kernel_fixture.rds")

# Floating-point accumulation differs slightly across CI platforms, so the oracle
# compares numeric payloads to near machine precision
expect_matches_fixture <- function(object, expected) {
  expect_equal(object, expected, tolerance = 1e-12)
}

test_that("quadratic system reproduces the frozen pre-refactor fixture exactly", {
  fix <- readRDS(fixture_path)
  moments <- compute_identification_moments(fix$w1, fix$w2, fix$z)
  qs <- build_quadratic_system(fix$gamma, fix$tau, moments)

  expect_matches_fixture(qs$quadratic, fix$qs_full$quadratic)
  expect_matches_fixture(qs$components$L_i, fix$qs_full$components$L_i)
  expect_matches_fixture(qs$components$V_i, fix$qs_full$components$V_i)
  expect_matches_fixture(qs$components$Q_i, fix$qs_full$components$Q_i)
})

test_that("subset-maturity quadratic system reproduces the frozen fixture exactly", {
  fix <- readRDS(fixture_path)
  moments <- compute_identification_moments(
    fix$w1, fix$w2, fix$z,
    maturities = c(1, 3)
  )
  qs <- build_quadratic_system(fix$gamma, fix$tau, moments)
  expect_matches_fixture(qs$quadratic, fix$qs_sub$quadratic)
  expect_matches_fixture(qs$components$L_i, fix$qs_sub$components$L_i)
  expect_matches_fixture(qs$components$V_i, fix$qs_sub$components$V_i)
  expect_matches_fixture(qs$components$Q_i, fix$qs_sub$components$Q_i)
})
