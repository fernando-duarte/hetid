# Tests for tau_star_fixed bracket logic: only a CERTIFIED-bounded tau
# (status == "bounded") counts as inside the bounded region, and only a
# CERTIFIED-unbounded tau brackets the transition. An "unreliable" finite solve
# (bounded flag TRUE but validity failed) is evidence of neither and is skipped.
# iters = 0 skips the bisection so the bracket is tested without solving.
# Run from the package root: Rscript scripts/utils/tests/test_tau_star_utils.R
source("scripts/utils/tau_star_utils.R")

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

mk_coarse <- function(tau, status, all_bounded, all_valid) {
  data.frame(
    tau = tau, total_width = NA_real_, all_bounded = all_bounded,
    all_valid = all_valid, status = status, grid = "coarse",
    stringsAsFactors = FALSE
  )
}

# An unreliable finite solve sits between a certified-bounded and an unbounded
# tau. The certificates are at 0.01 (bounded) and 0.03 (unbounded) and there is
# no information at 0.02, so the honest bracket is [0.01, 0.03] -> 0.02. Closing
# it at the unreliable tau instead would assert tau* < 0.02 on the strength of a
# solve that established nothing.
coarse_unreliable <- mk_coarse(
  tau = c(0.01, 0.02, 0.03),
  status = c("bounded", "unreliable", "unbounded"),
  all_bounded = c(TRUE, TRUE, FALSE),
  all_valid = c(TRUE, FALSE, FALSE)
)
res <- tau_star_fixed(gamma = NULL, moments = NULL, coarse_unreliable, iters = 0L)
check(
  "unreliable tau carries no evidence and is skipped (brackets 0.01-0.03 -> 0.02)",
  isTRUE(all.equal(res$tau_star, 0.02)) && isFALSE(res$capped)
)

# Every tau certified bounded -> tau* is censored at the grid maximum.
coarse_all <- mk_coarse(
  c(0.01, 0.02), c("bounded", "bounded"), c(TRUE, TRUE), c(TRUE, TRUE)
)
res_all <- tau_star_fixed(NULL, NULL, coarse_all, iters = 0L)
check(
  "all-certified sweep caps tau* at the grid maximum",
  isTRUE(res_all$capped) && isTRUE(all.equal(res_all$tau_star, 0.02))
)

# A clean bounded -> unbounded transition brackets between the two taus.
coarse_clean <- mk_coarse(
  c(0.02, 0.04), c("bounded", "unbounded"), c(TRUE, FALSE), c(TRUE, FALSE)
)
res_clean <- tau_star_fixed(NULL, NULL, coarse_clean, iters = 0L)
check(
  "clean transition brackets between 0.02 and 0.04 -> 0.03",
  isTRUE(all.equal(res_clean$tau_star, 0.03))
)

# tau_star_optimized 3-state oracle: like the fixed sweep above, only a
# CERTIFIED-unbounded tau brackets the transition; an uncertified tau carries no
# evidence. Live data never hits an uncertified optimum, so stub
# run_lambda_optimization to return a controlled set_status per tau and exercise
# the branch. run_tests.R runs each file in its own process, so the stub is
# local to this run.
with_status <- function(status_of) {
  run_lambda_optimization <<- function(gamma_start, moments, tau, ...) {
    list(set_status = status_of(tau[[1]]))
  }
}
g1 <- matrix(1) # ncol 1 -> n_comp 1

with_status(function(t) "uncertified")
check(
  "optimizer: uncertified start leaves tau* unlocated (NA)",
  is.na(tau_star_optimized(g1, NULL, whiten = NULL, tau_lo = 0.2, cap = 0.99)$tau_star)
)

with_status(function(t) if (t <= 0.4) "bounded" else "uncertified")
r_stall <- tau_star_optimized(g1, NULL, whiten = NULL, tau_lo = 0.2, cap = 0.99)
check(
  "optimizer: an uncertified stall censors tau* at the last certified-bounded tau",
  isTRUE(r_stall$capped) && r_stall$tau_star >= 0.4
)

with_status(function(t) if (t < 0.5) "bounded" else "unbounded")
r_trans <- tau_star_optimized(g1, NULL, whiten = NULL, tau_lo = 0.2, cap = 0.99, iters = 30L)
check(
  "optimizer: a certified-unbounded tau brackets the transition (~0.5)",
  isFALSE(r_trans$capped) && abs(r_trans$tau_star - 0.5) < 0.05
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
