# Panel-geometry and reporting checks for the residual-diagnostic figures. Run
# from the package root:
#   Rscript scripts-paper/tests/figures/residual_diagnostics/test_contracts.R
# The series algebra and the fail-closed guards live in the sibling suite; this
# one covers the quantile-quantile geometry the corner-to-corner reference line
# depends on, the closed-form reference density, the reported moments, and a
# render smoke test for both panels.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("config", "analysis_contract.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path(
  "log_variance", "figures", "residual_diagnostics", "data.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "residual_diagnostics", "plot.R"
))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# A fixture whose log-variance algebra is known in closed form: with theta the
# zero vector every fitted variance is one, so xi is exactly log(eps^2).
set.seed(704L)
n_obs <- 40L
pcr <- matrix(stats::rnorm(n_obs * 2L), nrow = n_obs)
colnames(pcr) <- c("l.pc1", "l.pc2")
x_mat <- logvar_design_matrix(pcr)
p_cols <- ncol(x_mat)
w2 <- matrix(stats::rnorm(n_obs), ncol = 1L)
b_point <- 0.3
eps <- stats::rnorm(n_obs)
w1 <- drop(w2 %*% b_point) + eps
inputs <- list(w1 = w1, w2 = w2, pcr = pcr, qtr = seq_len(n_obs))

zero_theta <- stats::setNames(numeric(p_cols), colnames(x_mat))
thetas <- list(ppml = zero_theta, harvey = zero_theta, logols = zero_theta)
series <- logvar_resid_diag_series(inputs, b_point, thetas)

# Quantile-quantile geometry: one shared theoretical axis, sorted empirical
# quantiles, and limits equal on both axes so y = x meets both frame corners.
qq <- logvar_resid_diag_qq_data(series)
ppml_qq <- qq[qq$estimator == "PPML", ]
check(
  "every estimator shares one theoretical axis",
  identical(
    qq$theoretical[qq$estimator == "PPML"],
    qq$theoretical[qq$estimator == "log-OLS"]
  )
)
check(
  "empirical quantiles are sorted",
  !is.unsorted(ppml_qq$empirical)
)
limits <- logvar_resid_diag_qq_limits(qq, 0.03)
span <- range(qq$theoretical, qq$empirical)
check(
  "limits are symmetric padding of the pooled range",
  max(abs(limits - (span + c(-1, 1) * 0.03 * diff(span)))) < 1e-12
)
check(
  "limits contain every plotted point, so nothing is clipped",
  limits[1L] < span[1L] && limits[2L] > span[2L]
)

# The reference curve is the exact log chi-square density, so it must integrate
# to one over a wide grid rather than merely look right.
grid <- seq(-30, 6, length.out = 20001L)
reference <- logvar_resid_diag_reference_density(grid)
mass <- sum(reference$y) * (grid[2L] - grid[1L])
check(
  "the log chi-square reference density integrates to one",
  abs(mass - 1) < 1e-6
)

# Moments, against a series whose distribution is known.
moments <- logvar_resid_diag_moments(series)
check(
  "moments carry one row per estimator",
  nrow(moments) == length(LOGVAR_RESID_DIAG_ESTIMATORS)
)
check(
  "the reported mean matches the series mean",
  abs(moments$mean[moments$estimator == "PPML"] - mean(log(eps^2))) < 1e-12
)
check(
  "the reference mean is E[log chi^2_1]",
  abs(attr(moments, "reference_mean") - (digamma(0.5) + log(2))) < 1e-12
)

# Render smoke test: both panels write a non-trivial SVG to a scratch path.
qq_path <- tempfile(fileext = ".svg")
density_path <- tempfile(fileext = ".svg")
logvar_resid_diag_qq_render(series, qq_path)
logvar_resid_diag_density_render(series, density_path)
check(
  "the quantile-quantile panel writes an SVG",
  file.exists(qq_path) && file.info(qq_path)$size > 1000
)
check(
  "the density panel writes an SVG",
  file.exists(density_path) && file.info(density_path)$size > 1000
)
unlink(c(qq_path, density_path))

.test$finish()
