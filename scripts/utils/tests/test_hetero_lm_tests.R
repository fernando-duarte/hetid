# Tests for hetero_lm_tests.R: the rk_rank_test joint-relevance rank test
# (M_Z construction, spectrum summaries, Newey-West chi-sq(1) statistic).
# The battery helpers (bp_lm_test, arch1_test, select_diagnostics_suite) are
# covered in test_hetero_test_utils.R.
# Run from the package root: Rscript scripts/utils/tests/test_hetero_lm_tests.R
source("scripts/utils/hetero_lm_tests.R")

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}

set.seed(42)
n <- 240
z <- rnorm(n)
vol <- exp(0.25 * z)
y2_full <- cbind(vol * rnorm(n), vol * rnorm(n), vol * rnorm(n))
rk <- rk_rank_test(y2_full, z)

# M_Z is the centered 1/T moment: diagonal equals Cov(z, y2_i^2), symmetric
zc <- z - mean(z)
check(
  "M_Z diagonal equals the centered Cov(Z, Y2^2) moment",
  all(abs(diag(rk$m_z) - (colMeans(zc * y2_full^2))) < 1e-12)
)
check("M_Z is symmetric", all(abs(rk$m_z - t(rk$m_z)) < 1e-14))

# spectrum summaries are internally consistent
check("det matches det(M_Z)", isTRUE(all.equal(rk$det, det(rk$m_z))))
check(
  "kappa and separation are ratios of the sorted spectrum",
  rk$kappa >= rk$sep && rk$sep >= 1 && rk$sv_min > 0
)
check(
  "smallest singular value matches svd",
  isTRUE(all.equal(rk$sv_min, min(svd(rk$m_z)$d)))
)

# statistic properties: chi-sq(1) p-value, documented automatic lag, and
# invariance to rescaling either input (s and lrv scale by the same power)
check("p-value is a probability", rk$p > 0 && rk$p < 1)
check("Newey-West lag follows the automatic rule", rk$lag == floor(4 * (n / 100)^(2 / 9)))
rk_scaled <- rk_rank_test(2 * y2_full, 3 * z)
check(
  "rk statistic is invariant to rescaling y2 and z",
  isTRUE(all.equal(rk_scaled$stat, rk$stat))
)

# power and size direction on seeded data: every column heteroskedastic in z
# gives full rank (small p); an unmoved third column gives a deficient null
# direction (large p)
check("full-rank alternative rejects", rk$p < 0.05)
y2_defic <- cbind(vol * rnorm(n), vol * rnorm(n), rnorm(n))
rk_defic <- rk_rank_test(y2_defic, z)
check("rank-deficient null is not rejected", rk_defic$p > 0.05)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
