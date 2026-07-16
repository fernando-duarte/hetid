# Contract and edge-case checks for the fitted-volatility adapter. Run from the
# package root: Rscript scripts-paper/tests/figures/fitted_volatility/test_contracts.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
source(paper_path("support", "identification", "profile_solver_core.R"))
source(paper_path("support", "identification", "profile_bounds_api.R"))
source(paper_path("log_variance", "core", "residual_map.R"))
source(paper_path("log_variance", "engine", "api.R"))
source(paper_path("log_variance", "figures", "fitted_volatility", "adapter.R"))
source(paper_path("log_variance", "figures", "fitted_volatility", "envelope.R"))
source(paper_path("log_variance", "figures", "fitted_volatility", "plot.R"))

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

phase_seen <- character(0)
phase_est <- list(
  metadata = list(
    estimator = "phase-fixture", target_functional = "theta_var",
    intercept_normalization = "fixture", sample_id = "sample-a",
    smoothness = "smooth", inner_solver = "closed form",
    response_scale = "variance", spec_id = "phase-fixture-v1",
    cold_start_rtol = 1e-8
  ),
  coef_labels = "a",
  fit_at_b = function(b, start = NULL, phase = NULL) {
    phase_seen <<- c(phase_seen, phase)
    coef <- stats::setNames(unname(b), "a")
    list(
      coef = coef, fit_status = "ok", converged = TRUE,
      objective = 0, score_norm = 0, convergence_code = 0L,
      diagnostics = list(), warm_start = coef
    )
  }
)
x_mat <- matrix(c(1, 2), ncol = 1L, dimnames = list(NULL, "a"))
labels <- c("date_0001", "date_0002")

direct <- logvar_fitted_vol_adapter(phase_est, x_mat, labels)
invisible(direct$fit_at_b(0.2, phase = "polish"))
cached <- logvar_fitted_vol_adapter(
  phase_est, x_mat, labels,
  source_cache = new.env(parent = emptyenv())
)
invisible(cached$fit_at_b(0.3, phase = "scan"))
invisible(cached$fit_at_b(0.3, phase = "cold_start"))
check(
  "adapter forwards service phases with and without a source cache",
  identical(phase_seen, c("polish", "scan", "cold_start"))
)

bad_sample <- tryCatch(
  {
    logvar_fitted_vol_adapter(
      phase_est, x_mat, labels,
      expected_sample_id = "sample-b"
    )
    FALSE
  },
  error = function(cond) TRUE
)
check("adapter rejects a mismatched source sample identity", bad_sample)

side_est <- phase_est
side_est$analyze_domain <- list(
  sides = function(qs, b_tab, scan) {
    list(
      lower_unbounded = c(TRUE, FALSE),
      upper_unbounded = c(FALSE, TRUE),
      unresolved_endpoints = character(0),
      closure_diagnostics = list(source = "fixture"),
      info = list(method = "fixture-side-hook")
    )
  }
)
side_adapter <- logvar_fitted_vol_adapter(side_est, x_mat, labels)
side_result <- side_adapter$analyze_domain$sides(
  NULL, NULL, list(min = c(0, 0), max = c(1, 1)), list()
)
check(
  "adapter preserves compatible estimator side diagnostics",
  identical(unname(side_result$lower_unbounded), c(TRUE, FALSE)) &&
    identical(unname(side_result$upper_unbounded), c(FALSE, TRUE)) &&
    identical(names(side_result$lower_unbounded), labels) &&
    length(side_result$unresolved_endpoints) == 0L
)

side_est$analyze_domain$sides <- function(qs, b_tab, scan) {
  list(
    lower_unbounded = FALSE, upper_unbounded = FALSE,
    unresolved_endpoints = character(0), closure_diagnostics = NULL,
    info = list()
  )
}
bad_side_adapter <- logvar_fitted_vol_adapter(side_est, x_mat, labels)
bad_sides <- bad_side_adapter$analyze_domain$sides(
  NULL, NULL, list(min = c(0, 0), max = c(1, 1)), list()
)
check(
  "a source side-axis mismatch fails every fitted endpoint closed",
  length(bad_sides$unresolved_endpoints) == 2L * length(labels)
)

edge_schema <- data.frame(
  lower = c(-Inf, 0), upper = c(Inf, 1000),
  lower_status = c("unbounded", "bounded"),
  upper_status = c("unbounded", "bounded"),
  lower_provenance = c(NA, "grid"),
  upper_provenance = c(NA, "grid")
)
edge_data <- logvar_fitted_vol_data(
  as.Date("2001-01-01") + c(0, 90), edge_schema, c(0, NA)
)
check(
  "extended-real log endpoints map monotonically to zero and infinity",
  identical(edge_data$volatility_lower[1], 0) &&
    is.infinite(edge_data$volatility_upper[1])
)
check(
  "finite-input exponential overflow is marked unavailable",
  is.na(edge_data$variance_upper[2]) &&
    identical(edge_data$upper_status[2], "unreliable")
)

no_point_rows <- edge_data
no_point_rows$lower_status <- "bounded"
no_point_rows$upper_status <- "bounded"
no_point_rows$volatility_lower <- c(0.5, 0.6)
no_point_rows$volatility_upper <- c(1.5, 1.6)
no_point_rows$volatility_point <- NA_real_
no_point <- logvar_fitted_vol_plot_data(no_point_rows)
check(
  "the no-point path draws no curve and discloses the missing red line",
  nrow(no_point$point) == 0L &&
    grepl("no red line", logvar_fitted_vol_caption(FALSE), fixed = TRUE)
)

empty_envelope <- list(
  metadata = list(estimator = "fixture", tau = 0.05),
  data = transform(
    no_point_rows,
    lower_status = "unreliable", upper_status = "unreliable"
  )
)
empty_message <- tryCatch(
  {
    logvar_fitted_vol_render(empty_envelope, tempfile(fileext = ".svg"))
    ""
  },
  error = conditionMessage
)
check(
  "renderer reports a clear error when no two-sided band is available",
  grepl("no two-sided bounded dates", empty_message, fixed = TRUE)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
