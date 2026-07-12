# Set identification of the SDF-news coefficients in the consumption-growth
# equation (Lewbel 2012 heteroskedasticity, relaxed-correlation form), per
# docs/lewbel_multivariate_set_identification.tex and the stage-08 paper-spec
# pattern. Same specification as ols_mean_eq_regression.R -- Y1 = consumption
# growth, common conditioning X_t = (1, lagged expected-SDF PCs), Y2 = the
# three SDF-news PCs, now treated as endogenous -- with the run_all.R
# instrument (column z_col of z_source()) as the single heteroskedasticity
# driver Z, applied to every news component (pair set = {news PC i} x {Z}).
# The quadratic-system assembly, profile-bound solver, and tau* sweep are
# reused from scripts/utils.
# Run via run_all.R after consumption_growth.R and sdf_pcs.R.

source("scripts/utils/identification_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/tau_star_utils.R")

# baseline slack and sweep cap (the pipeline's BASELINE_TAU / OPT_TAU_CAP
# values; admissible slack is [0, 1))
tau_baseline <- 0.05
tau_cap <- 0.99

# aligned estimation frame; complete cases truncate the sample to the
# instrument's span
set_id_data <- list(
  gr1_pcecc96,
  lag_expected_sdf_pc,
  sdf_news_pc,
  z_source()
) |>
  purrr::reduce(dplyr::full_join, by = "qtr") |>
  filter_window() |>
  tidyr::drop_na() |>
  dplyr::arrange(qtr)

y1_col <- hetid::HETID_CONSTANTS$CONSUMPTION_GROWTH_COL
x_cols <- value_cols(lag_expected_sdf_pc)
y2_cols <- value_cols(sdf_news_pc)

# residualize Y1 and every Y2 component on the same X_t:
# W1 = Y1 - X'beta1R and W2 = Y2 - beta2R X (the spec's residualized system)
fit_w1 <- stats::lm(stats::reformulate(x_cols, response = y1_col), data = set_id_data)
beta1r <- stats::coef(fit_w1)
w1 <- stats::residuals(fit_w1)
fit_w2 <- stats::lm(as.matrix(set_id_data[y2_cols]) ~ ., data = set_id_data[x_cols])
w2 <- stats::residuals(fit_w2)
beta2r <- t(stats::coef(fit_w2)) # I x p; rows = news PCs, cols match beta1r

# single de-meaned instrument with unit weight for every component
z <- set_id_data[[z_col]] - mean(set_id_data[[z_col]])
moments <- hetid::compute_identification_moments(
  w1, w2, matrix(z, ncol = 1, dimnames = list(NULL, z_col))
)
gamma <- matrix(1, 1, length(y2_cols))

qs_set <- build_pipeline_quadratic_system(
  gamma, rep(tau_baseline, length(y2_cols)), moments
)
qs0 <- build_pipeline_quadratic_system(gamma, rep(0, length(y2_cols)), moments)

# theta: profile bounds at the baseline slack, closed-form point at tau = 0
theta_bounds <- solve_all_profile_bounds(qs_set$quadratic)
point0 <- solve_point_identification(qs0$components)

# critical slack tau*: the bounded -> unbounded transition of the joint set
tau_sweep <- sweep_fixed_gamma(gamma, moments, seq(0, tau_cap, by = 0.005), "coarse")
tau_star <- tau_star_fixed(gamma, moments, tau_sweep)

# OLS benchmark on the identical sample (Y2 treated as exogenous)
ols_fit <- stats::lm(
  stats::reformulate(c(x_cols, y2_cols), response = y1_col),
  data = set_id_data
)

# honest three-state side status for a profile- or functional-bound pair
bound_status <- function(bounded, valid) {
  if (!bounded) "unbounded" else if (valid) "bounded" else "unreliable"
}

theta_table <- data.frame(
  coef = y2_cols,
  ols = unname(stats::coef(ols_fit)[y2_cols]),
  point = if (is.null(point0)) NA_real_ else point0$theta,
  set_lower = theta_bounds$lower,
  set_upper = theta_bounds$upper,
  status = mapply(
    bound_status,
    theta_bounds$bounded_lower & theta_bounds$bounded_upper,
    theta_bounds$valid_lower & theta_bounds$valid_upper
  ),
  row.names = NULL
)

# design-coefficient recovery beta1(theta) = beta1R - beta2R' theta: point at
# tau = 0, interval over the baseline set via the linear-functional solver
beta1_point <- if (is.null(point0)) {
  stats::setNames(rep(NA_real_, length(beta1r)), names(beta1r))
} else {
  hetid::recover_structural_coefficients(beta1r, beta2r, point0$theta)
}
beta1_table <- do.call(rbind, lapply(names(beta1r), function(p) {
  fmin <- solve_linear_functional_bound(qs_set$quadratic, beta2r[, p], "min")
  fmax <- solve_linear_functional_bound(qs_set$quadratic, beta2r[, p], "max")
  data.frame(
    coef = p,
    ols = unname(stats::coef(ols_fit)[p]),
    point = unname(beta1_point[p]),
    set_lower = unname(beta1r[p]) - fmax$bound,
    set_upper = unname(beta1r[p]) - fmin$bound,
    status = bound_status(fmin$bounded && fmax$bounded, fmin$valid && fmax$valid),
    row.names = NULL
  )
}))

# relevance and conditioning diagnostics: Cov(Z, W2_i^2) is what gives the
# constraints curvature (the het-tests note's "fuel"), kappa(Q) the joint
# conditioning of the tau = 0 system, cor(W1, W2) the endogeneity motivation
relevance <- data.frame(
  component = y2_cols,
  cor_z_w2sq = vapply(y2_cols, \(i) stats::cor(z, w2[, i]^2), numeric(1)),
  t_z_w2sq = vapply(
    y2_cols,
    \(i) summary(stats::lm(w2[, i]^2 ~ z))$coefficients[2, 3],
    numeric(1)
  ),
  cor_w1_w2 = vapply(y2_cols, \(i) stats::cor(w1, w2[, i]), numeric(1)),
  row.names = NULL
)

set_id_mean_eq <- list(
  sample = list(n = nrow(set_id_data), span = range(set_id_data$qtr)),
  tau_baseline = tau_baseline,
  theta_table = theta_table,
  beta1_table = beta1_table,
  theta_point_cond = if (is.null(point0)) NA_real_ else point0$cond,
  tau_star = tau_star$tau_star,
  tau_star_capped = tau_star$capped,
  tau_sweep = tau_sweep,
  relevance = relevance,
  w2_cor = stats::cor(w2),
  # aligned system pieces, kept for the downstream heteroskedasticity tests
  w1 = w1, y1 = set_id_data[[y1_col]], y2 = as.matrix(set_id_data[y2_cols]), z = z,
  moments = moments,
  gamma = gamma,
  ols_fit = ols_fit
)

cat(
  sprintf("set identification (Z = %s): N =", z_col), set_id_mean_eq$sample$n,
  "over", format(set_id_mean_eq$sample$span[1]), "to",
  format(set_id_mean_eq$sample$span[2]),
  "\n  tau* =", signif(set_id_mean_eq$tau_star, 3),
  if (set_id_mean_eq$tau_star_capped) "(capped at sweep max)" else "",
  " kappa(Q) =", signif(set_id_mean_eq$theta_point_cond, 3), "\n"
)
print(set_id_mean_eq$theta_table, digits = 3)
print(set_id_mean_eq$relevance, digits = 3)

rm(
  set_id_data, y1_col, x_cols, y2_cols, fit_w1, beta1r, w1, fit_w2, w2, beta2r,
  z, moments, gamma, qs_set, qs0, theta_bounds, point0, tau_sweep, tau_star,
  ols_fit, bound_status, theta_table, beta1_point, beta1_table, relevance,
  tau_baseline, tau_cap
)
