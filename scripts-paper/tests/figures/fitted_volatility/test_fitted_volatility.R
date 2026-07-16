# Offline checks for the estimator-agnostic fitted-volatility envelope and
# renderer. Run from the package root:
#   Rscript scripts-paper/tests/figures/fitted_volatility/test_fitted_volatility.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
source(repo_path("scripts", "utils", "profile_bounds_core.R"))
source(repo_path("scripts", "utils", "profile_bounds.R"))
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

# Smooth diagonal coefficient map theta(b) = (b, b). The first design row
# cancels the two coefficients, exposing why a Cartesian coefficient hull is
# not a valid fitted-value envelope.
base_calls <- 0L
base_est <- list(
  metadata = list(
    estimator = "fixture", target_functional = "theta_var",
    intercept_normalization = "fixture", sample_id = "fixture-sample",
    smoothness = "smooth", inner_solver = "closed form",
    response_scale = "variance", spec_id = "fixture-v1",
    cold_start_rtol = 1e-8
  ),
  coef_labels = c("a", "b"),
  fit_at_b = function(b, start = NULL) {
    base_calls <<- base_calls + 1L
    coef <- stats::setNames(rep(unname(b), 2L), c("a", "b"))
    list(
      coef = coef, fit_status = "ok", converged = TRUE,
      objective = 0, score_norm = 0, convergence_code = 0L,
      diagnostics = list(), warm_start = coef
    )
  },
  jacobian_at_b = function(b, fit = NULL) {
    stopifnot(identical(names(fit$coef), c("a", "b")))
    matrix(c(1, 1), nrow = 2L, dimnames = list(c("a", "b"), NULL))
  }
)
x_mat <- rbind(c(1, -1), c(1, 1), c(1, 0))
colnames(x_mat) <- c("a", "b")
qtr <- as.Date("2000-01-01") + c(0, 90, 181)
labels <- sprintf("date_%04d", seq_along(qtr))
source_cache <- new.env(parent = emptyenv())
adapter <- logvar_fitted_vol_adapter(
  base_est, x_mat, labels,
  source_cache = source_cache
)

# Projection preserves the source fit, warm start, and analytic Jacobian.
fit <- adapter$fit_at_b(0.25, phase = "scan")
jac <- adapter$jacobian_at_b(0.25, fit)
check(
  "adapter projects coefficients onto every dated design row",
  max(abs(unname(fit$coef) - c(0, 0.5, 0.25))) < 1e-12
)
check(
  "adapter projects the source Jacobian without overwriting its fit",
  max(abs(drop(jac) - c(0, 2, 1))) < 1e-12 &&
    identical(unname(fit$functional_source_coef), c(0.25, 0.25))
)
check(
  "adapter retains the variance-estimator warm start",
  identical(unname(fit$warm_start), c(0.25, 0.25))
)

# Source-cache hits accelerate ordinary phases; a cold replication bypasses it.
invisible(adapter$fit_at_b(0.25, phase = "scan"))
calls_after_cache <- base_calls
invisible(adapter$fit_at_b(0.25, phase = "cold_start"))
check(
  "source cache is reused while cold-start replication refits",
  calls_after_cache == 1L && base_calls == 2L
)

# The exact one-dimensional set b^2 <= 1 has analytic fitted-volatility bounds.
qs <- list(A_i = list(matrix(1, 1, 1)), b_i = list(0), c_i = -1)
b_tab <- data.frame(
  coef = "b", set_lower = -1, set_upper = 1, status = "bounded"
)
envelope <- logvar_fitted_vol_envelope(
  base_est, qtr, x_mat, qs, b_tab,
  b_seed = 0, b_point = 0, tau = 0.05,
  source_cache = source_cache,
  max_grid_points = 41L, max_fit_evals = 5000L
)
expected_lower <- c(1, exp(-1), exp(-0.5))
expected_upper <- c(1, exp(1), exp(0.5))
check(
  "engine profiles the datewise fitted-volatility extrema over the joint set",
  max(abs(envelope$data$volatility_lower - expected_lower)) < 1e-5 &&
    max(abs(envelope$data$volatility_upper - expected_upper)) < 1e-5
)
check(
  "Lewbel-point fitted volatility lies inside every two-sided band",
  all(envelope$data$volatility_point == 1) &&
    all(envelope$data$volatility_lower <= envelope$data$volatility_point) &&
    all(envelope$data$volatility_point <= envelope$data$volatility_upper)
)
check(
  "attaining arguments and side provenance remain in the result schema",
  all(c("arg_lower", "arg_upper", "lower_provenance", "upper_provenance") %in%
    names(envelope$schema)) && "domain_info" %in% names(envelope)
)

# The cancelling row is degenerate despite nondegenerate marginal coefficient
# intervals; rectangular interval arithmetic would fabricate a wide band.
naive_hull <- exp(0.5 * c(-2, 2))
check(
  "joint profiling avoids the unattainable coefficient-hull envelope",
  diff(range(
    envelope$data$volatility_lower[1],
    envelope$data$volatility_upper[1]
  )) < 1e-10 && diff(naive_hull) > 1
)

# Plot preparation assigns separate groups on either side of an unreliable gap.
gap_rows <- envelope$data
gap_rows$lower_status[2] <- "unreliable"
gap_rows$volatility_point[2] <- NA_real_
plot_data <- logvar_fitted_vol_plot_data(gap_rows)
check(
  "unreliable dates split rather than bridge the ribbon",
  nrow(plot_data$band) == 2L && length(unique(plot_data$band$run)) == 2L
)
check(
  "missing point values split rather than bridge the point curve",
  nrow(plot_data$point) == 2L && length(unique(plot_data$point$run)) == 2L
)

paths <- vapply(
  c("ppml", "harvey"),
  function(estimator) logvar_fitted_vol_path(out_dir, estimator),
  character(1)
)
check(
  "canonical output paths keep the two estimator figures distinct",
  identical(
    unname(paths),
    c(
      artifact_path("ppml_fitted_volatility_figure"),
      artifact_path("harvey_fitted_volatility_figure")
    )
  )
)

# The renderer produces a nonempty PDF from the generic envelope object.
pdf_path <- tempfile(fileext = ".pdf")
logvar_fitted_vol_render(envelope, pdf_path)
check(
  "generic renderer writes a nonempty fitted-volatility PDF",
  file.exists(pdf_path) && file.info(pdf_path)$size > 0
)
unlink(pdf_path)

# Design columns are a hard contract with the source estimator coefficient axis.
bad_x <- x_mat
colnames(bad_x) <- c("b", "a")
bad_design <- tryCatch(
  {
    logvar_fitted_vol_adapter(base_est, bad_x, labels)
    FALSE
  },
  error = function(cond) TRUE
)
check("adapter rejects a misaligned variance-equation design", bad_design)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
