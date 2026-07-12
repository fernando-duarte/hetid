# Tests for set_id_inference.R: the percentile band, the Imbens-Manski
# critical value, and the per-coefficient endpoint-inference assembly.
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
  "width 0 gives the two-sided normal critical value",
  abs(im_critical(0, 1, 1, alpha = 0.10) - qnorm(0.95)) < 1e-6
)
# a very wide set needs only one-sided protection at each endpoint
check(
  "wide set gives the one-sided normal critical value",
  abs(im_critical(100, 1, 1, alpha = 0.10) - qnorm(0.90)) < 1e-4
)
# degenerate standard errors fall back to the one-sided value
check(
  "zero standard error falls back to the one-sided value",
  identical(im_critical(0.3, 0, 0, alpha = 0.10), qnorm(0.90))
)
# the larger endpoint standard error is the binding one
check(
  "critical value is symmetric in the two standard errors",
  identical(im_critical(1, 0.5, 2, 0.10), im_critical(1, 2, 0.5, 0.10))
)

# boot_band summarizes the finite draws only
b <- boot_band(c(1:99, Inf, NA))
check("band drops non-finite draws", b[["n"]] == 99)
check("band median is the finite-draw median", b[["median"]] == 50)
check("band is the 5th-95th percentile pair", b[["p05"]] < b[["p95"]])

# endpoint_inference on hand-built draws with a known spread
set.seed(1)
n_reps <- 400L
lower <- cbind(rnorm(n_reps, -1, 0.1), rnorm(n_reps, -2, 0.2))
upper <- cbind(rnorm(n_reps, 1, 0.1), rep(NA_real_, n_reps))
tab <- data.frame(
  coef = c("a", "b"), set_lower = c(-1, -2), set_upper = c(1, 2),
  status = c("bounded", "bounded"), stringsAsFactors = FALSE
)
inf <- endpoint_inference(lower, upper, tab, alpha = 0.10)
check("endpoint se close to the draw spread", abs(inf$se_lower[1] - 0.1) < 0.02)
check(
  "IM interval strictly contains the exact set",
  inf$im_lower[1] < -1 && inf$im_upper[1] > 1
)
check(
  "IM interval is tighter than a naive two-sided outer set",
  inf$im_upper[1] < 1 + qnorm(0.95) * inf$se_upper[1]
)
check(
  "no finite draw pairs disables the interval",
  is.na(inf$im_lower[2]) && inf$n_finite[2] == 0
)
uncert <- tab
uncert$status <- c("unbounded", "bounded")
check(
  "uncertified full-sample rows get no interval",
  is.na(endpoint_inference(lower, upper, uncert, 0.10)$im_lower[1])
)
degen <- data.frame(
  coef = "c", set_lower = 0.5, set_upper = 0.5, status = "bounded",
  stringsAsFactors = FALSE
)
degen_draws <- matrix(rnorm(50, 0.5, 0.01), ncol = 1)
check(
  "degenerate width-0 rows get no interval",
  is.na(endpoint_inference(degen_draws, degen_draws, degen, 0.10)$im_lower)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) stop("test_set_id_inference failures")
