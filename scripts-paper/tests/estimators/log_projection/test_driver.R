# Offline checks for the regularized log-projection driver: the OLS reference
# reproduces the naive residuals (Frisch-Waugh-Lovell), display-set nesting
# violations demote without moving values, the uncertified Fuller hook marks
# every side unresolved on the engine paths, endpoint diagnostics round-trip
# through the typed CSV writer, and sourcing the driver offline defines
# helpers only. Run from the package root:
#   Rscript scripts-paper/tests/estimators/log_projection/test_driver.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "engine", "api.R"))
paper_source_once(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
paper_source_once(paper_path(
  "log_variance", "estimators", "log_projection", "run_sets.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "adapter.R"
))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

check(
  "sourcing the driver offline defines helpers and runs nothing",
  is.function(logvar_log_projection_sets) &&
    !exists("log_var_eq_log_plus") && !exists("log_var_eq_log_projection_tuning")
)

set.seed(11)
n_obs <- 60L
z <- cbind(1, rnorm(n_obs))
news <- cbind(bN1 = rnorm(n_obs), bN2 = rnorm(n_obs))
y <- drop(news %*% c(0.4, -0.2)) + rnorm(n_obs)
w1 <- drop(stats::lm.fit(z, y)$residuals)
w2 <- stats::lm.fit(z, news)$residuals
colnames(w2) <- colnames(news)
b_ols <- stats::lm.fit(cbind(z, news), y)$coefficients[3:4]
check(
  "the OLS news coefficients reproduce the naive residuals (FWL)",
  isTRUE(all.equal(
    unname(drop(w1 - w2 %*% b_ols)),
    unname(stats::lm.fit(cbind(z, news), y)$residuals),
    tolerance = 1e-10
  ))
)

# nesting: a lower bound that rises from tau = 0.05 to 0.1 is demoted in place
nest_result <- function(tau, lower) {
  sch <- data.frame(
    coef = "(Intercept)", tau = tau, lower = lower, upper = 1,
    lower_status = "bounded", upper_status = "bounded",
    stringsAsFactors = FALSE
  )
  list(schema = sch, table = data.frame(
    coef = "(Intercept)", set_lower = lower, set_upper = 1, status = "bounded"
  ))
}
nested <- logvar_log_projection_nesting(list(
  a = nest_result(0.05, -1), b = nest_result(0.1, -0.5)
))
check(
  "a nesting violation demotes the looser side without moving its value",
  identical(nested$results$b$schema$lower_status, "unreliable") &&
    identical(nested$results$b$schema$lower, -0.5) &&
    identical(nested$results$b$table$status, "unreliable") &&
    identical(nested$results$a$table$status, "bounded") &&
    nrow(nested$violations) == 1L
)

# the uncertified Fuller hook, on the estimator and through the envelope adapter
u <- rep(c(-1, 1), 8L)
v <- rep(c(-1, -1, 1, 1), 4L)
prep_bad <- hetid::prepare_log_projection(
  v, cbind(a = u, b = u + 2^-40 * v), cbind(l.pc1 = rnorm(16L)), 1:16, 1:16
)
est_bad <- logvar_log_projection_estimator(
  prep_bad, "log_fuller", "log_fuller", 1, "s", LOGVAR_LOG_PROJECTION_CONTROL
)
sides <- est_bad$analyze_domain$sides(NULL, NULL, NULL, NULL)
check(
  "an uncertified Fuller estimator leaves every endpoint unresolved",
  !prep_bad$scale_lower_certified &&
    length(sides$unresolved_endpoints) == 2L * length(est_bad$coef_labels) &&
    identical(sides$info$reason, "fuller_scale_uncertified")
)
dates <- paste0("d", 1:5)
adapted <- logvar_fitted_vol_domain(est_bad, dates)$sides(NULL, NULL, NULL, NULL)
check(
  "the envelope adapter keeps every date unresolved and the reason visible",
  length(adapted$unresolved_endpoints) == 2L * length(dates) &&
    identical(adapted$info$reason, "fuller_scale_uncertified")
)
prep_ok <- hetid::prepare_log_projection(w1, w2, cbind(l.pc1 = rnorm(n_obs)), 1:60, 1:60)
check(
  "a certified Fuller estimator and every additive estimator carry no hook",
  is.null(logvar_log_projection_estimator(
    prep_ok, "log_fuller", "log_fuller", 1, "s", LOGVAR_LOG_PROJECTION_CONTROL
  )$analyze_domain) &&
    is.null(logvar_log_projection_estimator(
      prep_bad, "log_plus", "log_plus", 1, "s", LOGVAR_LOG_PROJECTION_CONTROL
    )$analyze_domain)
)

# endpoint diagnostics: a fabricated reconciled map through the typed writer
est_ok <- logvar_log_projection_estimator(
  prep_ok, "log_fuller", "log_fuller", 1, "s", LOGVAR_LOG_PROJECTION_CONTROL
)
labels <- est_ok$coef_labels
sch <- data.frame(
  coef = labels, tau = 0.05, lower = c(-1, 0.1), upper = c(1, 0.2),
  lower_status = c("unreliable", "bounded"), upper_status = "bounded",
  lower_provenance = "primary", upper_provenance = "primary",
  lower_constraint_residual = -1e-3, upper_constraint_residual = -1e-3,
  stringsAsFactors = FALSE
)
sch$arg_lower <- I(list(c(0.1, 0.1), c(NA_real_, NA_real_)))
sch$arg_upper <- I(list(c(0.2, -0.1), c(0.3, 0)))
mapped <- list(
  estimator = est_ok, multiplier = 1,
  final = list(list(schema = sch)),
  audit = data.frame(
    tau = 0.05, coef = labels[[1L]], side = "lower", reason = "endpoint_moved",
    delta = 0.01, tol = 1e-4, stringsAsFactors = FALSE
  )
)
rows <- logvar_log_projection_endpoint_rows(
  mapped, prep_ok, list(b_ref = b_ols, b_point = b_ols, point_feasible = TRUE)
)
csv_path <- tempfile(fileext = ".csv")
written <- tryCatch(
  paper_write_typed_csv(rows, csv_path, "log projection endpoints"),
  error = function(e) e
)
check(
  "endpoint rows keep evaluation and endpoint status apart and round-trip",
  !inherits(written, "error") &&
    identical(rows$endpoint_status[[1L]], "unreliable") &&
    identical(rows$evaluation_status[[1L]], "ok") &&
    identical(rows$reason[[1L]], "endpoint_moved") &&
    is.na(rows$evaluation_status[[3L]]) &&
    all(c("theta0_(Intercept)", "b_bN1", "s_hat", "c_T") %in% names(rows)) &&
    identical(sum(rows$role == "point"), 1L)
)
unlink(csv_path)

.test$finish()
