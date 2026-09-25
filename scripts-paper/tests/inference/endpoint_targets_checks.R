# Paper adapter and published cell checks. Pure root/search oracles now live in
# tests/testthat/test-bootstrap_target_algebra.R alongside the package kernels.
source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("support", "inference_post", "endpoint_target_cells.R"))
paper_source_once(paper_path("support", "inference_post", "endpoint_point_statistic.R"))
set.seed(20260730L)
et_alpha <- PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha
et_tol <- PAPER_ANALYSIS_CONTRACT$inference$target_p_lambda_tolerance
et_stability <- PAPER_ANALYSIS_CONTRACT$inference$stability_share
et_b <- 100L
et_min_reps <- boot_min_reps(et_b)
et_pool <- rep(TRUE, et_b)
et_pass <- function(label) cat(sprintf("PASS  %s\n", label))
et_reference_critical <- function(x, alpha) {
  sort(x)[min(length(x), ceiling((length(x) + 1) * (1 - alpha)))]
}
paper_source_once(paper_path("tests", "inference", "endpoint_target_cell_checks.R"))
# The package budget remains useful for callers; paper policy still converges
# on wide-credit cells that legitimately need more than the default evaluations.
local({
  set.seed(11)
  z <- rnorm(100)
  w <- 0.6 * z + 0.8 * rnorm(100)
  m <- function(x) matrix(x, 100, 1, dimnames = list(NULL, "wide"))
  draws <- list(
    lower = m(z), upper = m(30000 - w),
    lower_status = m("bounded"), upper_status = m("bounded")
  )
  full <- data.frame(
    coef = "wide", lower = 0, upper = 30000,
    lower_status = "bounded", upper_status = "bounded"
  )
  budget <- hetid::bootstrap_set_interval(full, draws, "pointwise", 0.1, 50, 0.85)
  full$set_lower <- full$lower
  full$set_upper <- full$upper
  out <- endpoint_target_table(draws, full, 0.1, 50, 0.85, 1e-4)
  stopifnot(
    budget$summary$search_stop == "max_evals", out$c_p_evals > 10000,
    out$c_p_gap <= 1e-4, out$reason == "reported"
  )
  et_pass("paper policy converges beyond the package default search budget")
})
cat("endpoint_targets_checks: PASS\n")
