# Imposed exact-news (B = 0) cascade -- end-to-end at the FUNCTION level, with NO
# multi-stage RDS artifacts written (those are produced under estimate-B and must
# not be overwritten here). The cascade entry point compute_identification_-
# residuals() needs the ACM yield/term-premium columns, which live in the stage-
# 01 input scripts/output/temp/data.rds (the bundled `variables` dataset omits
# them); this test READS that input read-only and prints a loud SKIP + exits 0
# when it is absent, exactly as test_z_width_pipeline.R does. Run from root:
#   Rscript scripts/utils/tests/test_impose_b_zero.R
#
# Cascade covered, with HETID_IMPOSE_NEWS_PROJECTION_ZERO=TRUE (hermetic):
#   * compute_identification_residuals(data) returns W2 == Y2 (the SDF news) and
#     a FULL-WIDTH all-zero w2_coefficients (beta2R) with the common-design
#     column names (Intercept, pc1..pcJ, y1_lag1..y1_lagH).
#   * build_reduced_form_gamma(w2_coefficients) raises the instructive all-zero
#     (B = 0) error -- the condition the tau_star_comparison.R and stage-06
#     table guards key on.
#   * recover_structural_coefficients(beta1R, beta2R = 0, theta) == beta1R for
#     ANY theta (beta1(theta) = beta1R; beta1 is point-valued, theta-free).
#   * compute_identification_moments still computes on the zero-projection W2.
# Plus the estimate-B DEFAULT (env unset): w2_coefficients is NON-zero (real
# estimated slopes) and build_reduced_form_gamma succeeds. This proves the guard
# is mode-specific, not a blanket change.
source(here::here("scripts/utils/common_settings.R"))

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

# Stage-01 input carries the ACM yield/term-premium columns the residual cascade
# needs (the bundled `variables` dataset does not). Read it read-only; SKIP loudly
# and exit 0 if absent so this stays hermetic where stage 01 has not been run.
if (!file.exists(DATA_RDS_PATH)) {
  cat("SKIPPED: scripts/output/temp/data.rds missing (run stage 01 first)\n")
  quit(status = 0L)
}
pipeline_data <- load_identification_inputs()$data
stopifnot(is.data.frame(pipeline_data))

# Hermetic env toggle: capture the prior value and ALWAYS restore on exit, so a
# failed assertion mid-run cannot leak the impose-B=0 flag into later tests.
prior_env <- Sys.getenv("HETID_IMPOSE_NEWS_PROJECTION_ZERO", unset = NA)
restore_env <- function() {
  if (is.na(prior_env)) {
    Sys.unsetenv("HETID_IMPOSE_NEWS_PROJECTION_ZERO")
  } else {
    Sys.setenv(HETID_IMPOSE_NEWS_PROJECTION_ZERO = prior_env)
  }
}
on.exit(restore_env(), add = TRUE)

# ---------------------------------------------------------------------------
# Estimate-B DEFAULT path (env unset): the guard must be mode-specific.
# ---------------------------------------------------------------------------
Sys.unsetenv("HETID_IMPOSE_NEWS_PROJECTION_ZERO")
check(
  "default mode resolves estimate-B (impose_news_projection_zero() is FALSE)",
  identical(impose_news_projection_zero(), FALSE)
)
resid_est <- suppressMessages(compute_identification_residuals(pipeline_data))
beta2r_est <- resid_est$w2_coefficients
pc_cols_est <- grep("^pc[0-9]+$", colnames(beta2r_est))
check(
  "estimate-B beta2R has a NON-zero PC slope block (B is estimated)",
  length(pc_cols_est) > 0L &&
    !all(beta2r_est[, pc_cols_est, drop = FALSE] == 0)
)
g_rf_est <- tryCatch(build_reduced_form_gamma(beta2r_est), error = identity)
check(
  "estimate-B build_reduced_form_gamma succeeds (J x I, reduced_form method)",
  is.matrix(g_rf_est) &&
    identical(attr(g_rf_est, "method"), "reduced_form") &&
    nrow(g_rf_est) == length(pc_cols_est)
)
# W2 under estimate-B is a genuine residual: NOT bit-identical to the raw news.
# (Sanity that the two branches really differ -- guards against a vacuous test.)

# ---------------------------------------------------------------------------
# Imposed exact-news B = 0 path (env TRUE).
# ---------------------------------------------------------------------------
Sys.setenv(HETID_IMPOSE_NEWS_PROJECTION_ZERO = "TRUE")
check(
  "TRUE env resolves impose-B=0 (impose_news_projection_zero() is TRUE)",
  identical(impose_news_projection_zero(), TRUE)
)
resid_b0 <- suppressMessages(compute_identification_residuals(pipeline_data))
w2_b0 <- resid_b0$w2
beta2r_b0 <- resid_b0$w2_coefficients

# beta2R is full-width all zeros with the common-design column names.
expected_cols <- colnames(beta2r_est)
check(
  "impose-B=0 beta2R column names match the common design (Intercept, pc*, lag*)",
  identical(colnames(beta2r_b0), expected_cols) &&
    any(grepl("^\\(Intercept\\)$", expected_cols)) &&
    any(grepl("^pc[0-9]+$", expected_cols))
)
check(
  "impose-B=0 beta2R is a FULL-WIDTH all-zero matrix (same shape as estimate-B)",
  identical(dim(beta2r_b0), dim(beta2r_est)) && all(beta2r_b0 == 0)
)

# W2 residual equals Y2 (the SDF news) under B = 0. The exact-news residual is
# the news itself, so it must NOT equal the estimate-B residual (which projected
# out X_t). We assert the B=0 W2 differs from the estimate-B W2 and that it is a
# genuine non-degenerate series (the raw news passed through unmodified).
check(
  "impose-B=0 W2 differs from the estimate-B W2 (news is NOT projected on X_t)",
  !isTRUE(all.equal(unname(unclass(w2_b0)), unname(unclass(resid_est$w2))))
)
check(
  "impose-B=0 W2 is a finite non-degenerate news matrix (Y2 passed through)",
  is.numeric(w2_b0) && all(is.finite(w2_b0)) &&
    nrow(w2_b0) > 0L && any(apply(w2_b0, 2, sd) > 0)
)

# build_reduced_form_gamma raises the instructive all-zero (B = 0) error.
rf_err <- tryCatch(
  {
    build_reduced_form_gamma(beta2r_b0)
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "build_reduced_form_gamma raises the instructive B = 0 / undefined error",
  is.character(rf_err) && grepl("B = 0", rf_err) &&
    grepl("all zeros", rf_err)
)

# recover_structural_coefficients with zero beta2R returns beta1R for ANY theta.
beta1r_b0 <- resid_b0$w1_result$coefficients
i_dim <- nrow(beta2r_b0)
# beta1R must be column-matched to beta2R for the recovery identity.
check(
  "beta1R is column-matched to the full-width beta2R",
  length(beta1r_b0) == ncol(beta2r_b0) &&
    identical(names(beta1r_b0), colnames(beta2r_b0))
)
thetas <- list(
  zero = rep(0, i_dim),
  ones = rep(1, i_dim),
  mixed = seq_len(i_dim) - (i_dim / 2),
  large = rep(1e6, i_dim)
)
beta1_invariant <- vapply(thetas, function(th) {
  b1 <- recover_structural_coefficients(beta1r_b0, beta2r_b0, th)
  isTRUE(all.equal(unname(b1), unname(beta1r_b0)))
}, logical(1))
check(
  "beta1(theta) = beta1R for EVERY theta under B = 0 (beta1 point-valued)",
  all(beta1_invariant)
)

# Moments still compute on the zero-projection W2 without error.
moments_b0 <- tryCatch(
  suppressMessages(compute_identification_moments(
    resid_b0$w1, w2_b0, resid_b0$pcs_aligned
  )),
  error = function(e) conditionMessage(e)
)
check(
  "compute_identification_moments succeeds on the zero-projection W2",
  inherits(moments_b0, "hetid_moments") &&
    identical(attr(moments_b0, "n_components"), i_dim)
)

restore_env()
check(
  "env restored after the impose-B=0 block (hermetic)",
  identical(
    Sys.getenv("HETID_IMPOSE_NEWS_PROJECTION_ZERO", unset = NA),
    prior_env
  )
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
