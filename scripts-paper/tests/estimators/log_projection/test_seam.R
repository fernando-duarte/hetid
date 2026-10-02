# Offline checks for the seam between the package log projections and the
# set engine (scripts-paper/log_variance/estimators/log_projection/estimator.R
# and the log-OLS object built on it): the pipeline helpers reproduce the
# package map, the estimator closures match the package evaluation, the batch
# scan equals a brute-force scan and returns separated pools on request, a
# withheld Jacobian becomes a NaN matrix, and cache identities separate
# methods and multipliers. Run from the package root:
#   Rscript scripts-paper/tests/estimators/log_projection/test_seam.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "engine", "api.R"))
paper_source_once(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

set.seed(7)
n_mean <- 90L
n_vol <- 70L
z <- cbind(1, rnorm(n_mean))
news <- cbind(bN1 = rnorm(n_mean), bN2 = rnorm(n_mean))
w1_mean <- drop(stats::lm.fit(z, rnorm(n_mean) + news[, 1])$residuals)
w2_mean <- stats::lm.fit(z, news)$residuals
colnames(w2_mean) <- colnames(news)
qtr <- seq_len(n_mean)
vol_rows <- utils::tail(qtr, n_vol)
pcr <- scale(matrix(rnorm(n_vol * 2L), n_vol, 2L,
  dimnames = list(NULL, c("l.pc1", "l.pc2"))
), center = TRUE, scale = FALSE)
prep <- hetid::prepare_log_projection(w1_mean, w2_mean, pcr, qtr, vol_rows)
w1 <- prep$w1
w2 <- prep$w2
b0 <- c(bN1 = 0.3, bN2 = -0.2)

# the pipeline helpers that diagnostics still call are the package map
pkg <- hetid::evaluate_log_projection(prep, b0, "log")
proj <- logvar_projection(pcr)
check(
  "logvar_theta_hat equals the package log projection",
  isTRUE(all.equal(unname(logvar_theta_hat(b0, w1, w2, proj)),
    unname(pkg$coef),
    tolerance = 1e-12
  ))
)
check(
  "logvar_theta_jacobian equals the package Jacobian",
  isTRUE(all.equal(unname(logvar_theta_jacobian(b0, w1, w2, proj)),
    unname(pkg$jacobian),
    tolerance = 1e-12
  ))
)
v_mat <- cbind(1, pcr)
check(
  "the QR operator is the normal-equations operator to roundoff",
  isTRUE(all.equal(unname(proj), unname(solve(crossprod(v_mat), t(v_mat))),
    tolerance = 1e-10
  ))
)

# generic estimator closures against the package
est <- logvar_log_projection_estimator(
  prep, "logols", "log", 1, "sample", LOGVAR_LOGOLS_CONTROL
)
fit <- est$fit_at_b(b0)
check(
  "fit_at_b returns the package coefficients with an ok status",
  identical(fit$fit_status, "ok") && identical(fit$coef, pkg$coef)
)
check(
  "jacobian_at_b returns the package Jacobian",
  identical(est$jacobian_at_b(b0), pkg$jacobian)
)
grid <- as.matrix(expand.grid(bN1 = seq(-0.5, 0.5, 0.1), bN2 = seq(-0.5, 0.5, 0.1)))
dimnames(grid) <- NULL
scan <- est$scan_grid(grid)
brute <- vapply(
  seq_len(nrow(grid)), function(i) est$fit_at_b(grid[i, ])$coef,
  numeric(nrow(proj))
)
check(
  "the batch scan equals a brute-force scan",
  isTRUE(all.equal(scan$min, unname(apply(brute, 1, min)), tolerance = 1e-12)) &&
    isTRUE(all.equal(scan$max, unname(apply(brute, 1, max)), tolerance = 1e-12))
)
pooled <- logvar_log_projection_estimator(
  prep, "logols", "log", 1, "sample", LOGVAR_LOGOLS_CONTROL,
  pool_k = 5L
)$scan_grid(grid)
check(
  "pool_k > 1 returns separated pools for every coefficient",
  length(pooled$arg_min_pool) == nrow(proj) &&
    all(lengths(pooled$arg_min_pool) >= 1L) && is.null(scan$arg_min_pool)
)
tiny <- hetid::prepare_log_projection(
  w1_mean * 1e-310, w2_mean, pcr, qtr, vol_rows
)
est_tiny <- logvar_log_projection_estimator(
  tiny, "logols", "log", 1, "sample", LOGVAR_LOGOLS_CONTROL
)
jac_nan <- est_tiny$jacobian_at_b(b0 * 1e-310)
check(
  "a withheld Jacobian is a NaN matrix of the documented shape",
  identical(dim(jac_nan), c(nrow(proj), 2L)) && all(is.nan(jac_nan))
)
spec_of <- function(method, m) {
  logvar_log_projection_estimator(
    prep, "x", method, m, "sample", LOGVAR_LOGOLS_CONTROL
  )$metadata$spec_id
}
check(
  "spec_ids separate multipliers and agree for equal inputs",
  !identical(spec_of("log", 1), spec_of("log", 2)) &&
    identical(spec_of("log", 1), spec_of("log", 1))
)

# the log-OLS object keeps its own scan and divergence semantics; an integer
# system has exact zero residuals at b = 0
w1_int <- rep(c(-2, -1, 0, 1, 2, 0), 2L)
w2_int <- cbind(bN1 = rep(c(1, -1), 6L), bN2 = rep(c(1, 1, -1, -1, 0, 0), 2L))
pcr_int <- cbind(l.pc1 = rnorm(12L))
prep_int <- hetid::prepare_log_projection(w1_int, w2_int, pcr_int, 1:12, 1:12)
est_int <- logvar_logols_estimator(prep_int, 1:12, w1_int, w2_int, pcr_int)
check(
  "log-OLS returns domain_failure at an exact zero residual",
  identical(est_int$fit_at_b(c(0, 0))$fit_status, "domain_failure")
)
est_ols <- logvar_logols_estimator(prep, vol_rows, w1, w2, pcr)
ols_scan <- est_ols$scan_grid(rbind(c(-1, 0), c(1, 0), b0))
check(
  "log-OLS scan reports both-signs crossings and never counts failures",
  length(ols_scan$domain_info$cross_grid) > 0L && identical(ols_scan$n_fit_failures, 0L)
)
check(
  "log-OLS keeps its census hook",
  is.function(est_ols$analyze_domain$precheck) && is.function(est_ols$coef_objective)
)

.test$finish()
