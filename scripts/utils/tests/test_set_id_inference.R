# Tests for set_id_inference.R: percentile band, Imbens-Manski and Stoye
# critical values, robust scale and correlation, and the per-coefficient
# endpoint-inference assembly.
# Run from the package root: Rscript scripts/utils/tests/test_set_id_inference.R
source("scripts/utils/set_id_inference.R")

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

# im_critical: a width-0 set needs symmetric two-sided coverage
check(
  "IM width 0 gives the two-sided normal critical value",
  abs(im_critical(0, 1, 1, alpha = 0.10) - qnorm(0.95)) < 1e-6
)
check(
  "IM wide set gives the one-sided normal critical value",
  abs(im_critical(100, 1, 1, alpha = 0.10) - qnorm(0.90)) < 1e-4
)
check(
  "IM zero standard error falls back to the one-sided value",
  identical(im_critical(0.3, 0, 0, alpha = 0.10), qnorm(0.90))
)
check(
  "IM critical value is symmetric in the two standard errors",
  identical(im_critical(1, 0.5, 2, 0.10), im_critical(1, 2, 0.5, 0.10))
)

# bivariate-normal building block: independence factorizes, rho -> 1 collapses
# to the comonotone overlap, and the closed form matches the integral nearby
check(
  "pbvn factorizes at rho = 0",
  abs(pbvn_le_ge(1, -1, 0) - pnorm(1) * (1 - pnorm(-1))) < 1e-8
)
check(
  "pbvn at rho near 1 collapses to the comonotone overlap",
  abs(pbvn_le_ge(1, -1, 0.999999) - (pnorm(1) - pnorm(-1))) < 1e-6
)
check(
  "pbvn closed form is continuous with the integral across the cutover",
  abs(pbvn_le_ge(1.2, -0.4, 0.9995) - pbvn_le_ge(1.2, -0.4, 0.998)) < 5e-3
)

# stoye_critical: sits between the one- and two-sided values and agrees with
# the Imbens-Manski interpolation as the endpoints become comonotone -- the
# binding case, where the two one-sided misses are disjoint -- shortening as
# the endpoint errors decouple
c_im_ref <- im_critical(0.7, 1, 1, alpha = 0.10)
c_hi <- stoye_critical(0.7, 1, 1, rho = 0.999, alpha = 0.10)
c_lo <- stoye_critical(0.7, 1, 1, rho = 0, alpha = 0.10)
check("Stoye bounded by one- and two-sided", c_lo > qnorm(0.90) && c_lo < qnorm(0.95) + 0.02)
check("Stoye at rho near 1 matches Imbens-Manski", abs(c_hi - c_im_ref) < 5e-3)
check("comonotone endpoint errors are the binding case", c_hi > c_lo)
check(
  "Stoye wide set gives the one-sided value",
  abs(stoye_critical(100, 1, 1, rho = 0.5, alpha = 0.10) - qnorm(0.90)) < 1e-3
)
check(
  "Stoye degenerate scale falls back to the one-sided value",
  identical(stoye_critical(0.3, 0, 0, rho = 0.5, alpha = 0.10), qnorm(0.90))
)

# robust_scale: matches mad, immune to a wild draw, NA when underdetermined
set.seed(2)
v <- rnorm(200, 0, 0.1)
check("robust_scale equals mad on clean draws", identical(robust_scale(v), mad(v)))
check(
  "one wild draw leaves the robust scale in place",
  abs(robust_scale(c(v, 4000)) - mad(v)) < 0.02
)
check("robust_scale is NA for a single value", is.na(robust_scale(c(1, Inf))))

# robust_endpoint_cor: recovers a strong correlation despite one wild pair
set.seed(3)
base <- rnorm(200)
l <- base + rnorm(200, 0, 0.3)
u <- base + rnorm(200, 0, 0.3)
l2 <- c(l, -4000)
u2 <- c(u, 9000)
check(
  "robust correlation survives a wild pair that wrecks the raw one",
  robust_endpoint_cor(l2, u2) > 0.8 && cor(l2, u2) < 0.2
)
check("robust correlation is NA on too few pairs", is.na(robust_endpoint_cor(1:5, 1:5)))

# boot_band summarizes the finite draws only
b <- boot_band(c(1:99, Inf, NA))
check("band drops non-finite draws", b[["n"]] == 99)
check("band median is the finite-draw median", b[["median"]] == 50)
check("band is the 5th-95th percentile pair", b[["p05"]] < b[["p95"]])

# endpoint_inference on hand-built draws with a known spread and one outlier
set.seed(1)
n_reps <- 400L
lower <- cbind(rnorm(n_reps, -1, 0.1), rnorm(n_reps, -2, 0.2))
upper <- cbind(rnorm(n_reps, 1, 0.1), rep(NA_real_, n_reps))
lower[1, 1] <- -5000
tab <- data.frame(
  coef = c("a", "b"), set_lower = c(-1, -2), set_upper = c(1, 2),
  status = c("bounded", "bounded"), stringsAsFactors = FALSE
)
inf <- endpoint_inference(lower, upper, tab, alpha = 0.10)
check(
  "endpoint scale close to the clean spread despite the outlier",
  abs(inf$se_lower[1] - 0.1) < 0.03
)
check(
  "interval strictly contains the exact set",
  inf$ci_lower[1] < -1 && inf$ci_upper[1] > 1
)
check(
  "interval is tighter than a naive two-sided outer set",
  inf$ci_upper[1] < 1 + qnorm(0.95) * inf$se_upper[1] + 1e-9
)
check(
  "Stoye and IM critical values are both reported",
  is.finite(inf$c_im[1]) && is.finite(inf$c_stoye[1])
)
check(
  "no finite draw pairs disables the interval",
  is.na(inf$ci_lower[2]) && inf$n_finite[2] == 0
)
uncert <- tab
uncert$status <- c("unbounded", "bounded")
check(
  "uncertified full-sample rows get no interval",
  is.na(endpoint_inference(lower, upper, uncert, 0.10)$ci_lower[1])
)
degen <- data.frame(
  coef = "c", set_lower = 0.5, set_upper = 0.5, status = "bounded",
  stringsAsFactors = FALSE
)
degen_draws <- matrix(rnorm(50, 0.5, 0.01), ncol = 1)
check(
  "degenerate width-0 rows get no interval",
  is.na(endpoint_inference(degen_draws, degen_draws, degen, 0.10)$ci_lower)
)

# width uncertainty comes from the paired draws: a rigid set has zero width
# scale and a degenerate width band even when both endpoints move together
set.seed(5)
rigid_l <- rnorm(60)
rigid <- endpoint_inference(
  cbind(rigid_l), cbind(rigid_l + 2),
  data.frame(
    coef = "r", set_lower = 0, set_upper = 2, status = "bounded",
    stringsAsFactors = FALSE
  ),
  alpha = 0.10
)
check(
  "paired width scale is zero for a rigid set",
  rigid$se_width == 0 && rigid$se_lower > 0
)
check(
  "width band collapses to the rigid width",
  abs(rigid$width_p05 - 2) < 1e-9 && abs(rigid$width_p95 - 2) < 1e-9
)

# point_inference: robust two-sided interval for the tau = 0 point draws,
# suppressed below the shared half-the-draws gate
set.seed(4)
pd <- cbind(a = rnorm(100, 1, 0.2), b = c(rnorm(30), rep(NA_real_, 70)))
pt <- point_inference(c(1, 0), pd, alpha = 0.10)
check(
  "point interval is the two-sided robust interval",
  abs(pt$lower[1] - (1 - qnorm(0.95) * mad(pd[, "a"]))) < 1e-12
)
check(
  "point interval is suppressed below the half-draws gate",
  is.na(pt$lower[2]) && pt$n_finite[2] == 30 && is.finite(pt$se[2])
)
check("half-draws threshold is integer division", boot_min_reps(9L) == 4L)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) stop("test_set_id_inference failures")
