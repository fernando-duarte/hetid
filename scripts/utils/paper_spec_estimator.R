# Paper-spec estimator: from the I=1 / J=1 residuals, assemble everything the
# three for_paper tables need -- the identified set for theta (the price of SDF-
# news risk), the structural-coefficient intervals via beta1(theta), tau*, the
# heteroskedasticity (relevance) tests of W2 against the VFCI instrument, the
# relevance diagnostics that REPLACE kappa(Q) (vacuous at I=J=1), the endogeneity
# correlation, and the OLS benchmark. The moving-block bootstrap band lives in
# paper_spec_bootstrap.R.

# Scalar (I=1) identified-set status from the one-row profile bounds. The set
# grows from the tau=0 point, so when point ID exists it is non-empty; "empty"
# is therefore folded into "unreliable" (a failed/invalid solve) defensively.
.classify_set_status <- function(b, point_ok) {
  bounded <- isTRUE(b$bounded_lower) && isTRUE(b$bounded_upper)
  valid <- isTRUE(b$valid_lower) && isTRUE(b$valid_upper)
  finite <- is.finite(b$lower) && is.finite(b$upper)
  if (bounded && valid && finite) {
    if (b$width <= 1e-8) "point" else "interval"
  } else if (!bounded) {
    "unbounded"
  } else {
    "unreliable"
  }
}

compute_paper_spec_estimator <- function(resid, tau_set = BASELINE_TAU) {
  w1 <- resid$w1
  w2v <- as.numeric(resid$w2)
  zv <- as.numeric(resid$z)
  w2mat <- matrix(w2v, ncol = 1)
  zmat <- matrix(zv, ncol = 1, dimnames = list(NULL, "vfci_dm"))

  moments <- compute_identification_moments(w1, w2mat, zmat)
  gamma <- matrix(1, 1, 1)
  qs_set <- build_pipeline_quadratic_system(gamma, tau_set, moments)
  qs0 <- build_pipeline_quadratic_system(gamma, 0, moments)

  theta_bounds <- solve_all_profile_bounds(qs_set$quadratic) # one row (I=1)
  pt0 <- solve_point_identification(qs0$components) # NULL on rank failure
  theta_point <- if (is.null(pt0)) NA_real_ else pt0$theta
  set_status <- .classify_set_status(theta_bounds[1, ], !is.null(pt0))

  # OLS benchmark: structural equation treating Y2 as exogenous.
  ols_df <- data.frame(.y1 = resid$y1_level, resid$design_aligned, .y2 = resid$y2)
  ols_coef <- coef(lm(.y1 ~ ., data = ols_df))

  # beta1(theta) point recovery (tau = 0).
  beta2r <- resid$beta2r
  beta2r_mat <- matrix(beta2r, nrow = 1, dimnames = list("news_pc", names(beta2r)))
  beta1_point <- if (is.null(pt0)) {
    stats::setNames(rep(NA_real_, length(resid$beta1r)), names(resid$beta1r))
  } else {
    recover_structural_coefficients(resid$beta1r, beta2r_mat, theta_point)
  }

  # Coefficient table: design coefs (interval via the linear functional
  # beta1_p(theta) = beta1r_p - beta2r_p . theta) + the theta row.
  coef_row <- function(coef, ols, point, lower, upper, bounded, valid) {
    data.frame(
      coef = coef, ols = unname(ols), point = unname(point),
      set_lower = unname(lower), set_upper = unname(upper),
      bounded = isTRUE(bounded), valid = isTRUE(valid),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }
  design_rows <- lapply(names(resid$beta1r), function(p) {
    cp <- unname(beta2r[p])
    fmin <- solve_linear_functional_bound(qs_set$quadratic, cp, "min")
    fmax <- solve_linear_functional_bound(qs_set$quadratic, cp, "max")
    coef_row(
      p, ols_coef[p], beta1_point[p],
      resid$beta1r[p] - fmax$bound, resid$beta1r[p] - fmin$bound,
      fmin$bounded && fmax$bounded, fmin$valid && fmax$valid
    )
  })
  theta_row <- coef_row(
    "theta", ols_coef[".y2"], theta_point,
    theta_bounds$lower[1], theta_bounds$upper[1],
    theta_bounds$bounded_lower[1] && theta_bounds$bounded_upper[1],
    theta_bounds$valid_lower[1] && theta_bounds$valid_upper[1]
  )
  coef_table <- rbind(do.call(rbind, design_rows), theta_row)

  # tau*: largest common slack keeping the set bounded.
  coarse <- sweep_fixed_gamma(gamma, moments, seq(0, OPT_TAU_CAP, by = 0.005), "coarse")
  ts <- tau_star_fixed(gamma, moments, coarse)

  # Heteroskedasticity (relevance) tests of W2 on the VFCI instrument; mirror
  # stage 02's calling convention, each wrapped so a failure renders NA.
  suite_cfg <- select_diagnostics_suite(w2mat, zmat)
  lm_w2 <- lm(w2 ~ ., data = data.frame(w2 = w2v, vfci_dm = zv))
  suite <- tryCatch(
    perform_all_hetero_tests(lm_w2, "news_pc",
      tests = suite_cfg$suite_tests,
      gq_deflator = suite_cfg$gq_deflator, gq_alternative = suite_cfg$gq_alternative
    ),
    error = function(e) NULL
  )
  suite_pvals <- if (is.null(suite)) {
    NULL
  } else {
    cols <- grep("_pval$", names(suite), value = TRUE)
    stats::setNames(as.numeric(suite[1, cols]), sub("_pval$", "", cols))
  }
  hetero_pvals <- c(
    suite_pvals,
    Glejser = tryCatch(skedastic::glejser(lm_w2)$p.value, error = function(e) NA_real_),
    BPLM = tryCatch(bp_lm_test(w2v, zmat)$p_value, error = function(e) NA_real_),
    ARCH = tryCatch(arch1_test(w2v)$p_value, error = function(e) NA_real_)
  )

  # Relevance / conditioning diagnostics (replace the vacuous kappa(Q)).
  cov_z_w2sq <- mean(zv * w2v^2) - mean(zv) * mean(w2v^2)
  mean_fit <- summary(lm(w2v ~ zv))$coefficients
  relevance <- list(
    cov_z_w2sq = cov_z_w2sq,
    cor_z_w2sq = stats::cor(zv, w2v^2),
    cor_z2_w2 = stats::cor(zv^2, w2v),
    mean_slope = unname(mean_fit[2, 1]),
    mean_slope_t = unname(mean_fit[2, 3]),
    cor_w1_w2 = stats::cor(w1, w2v)
  )

  list(
    moments = moments, gamma = gamma,
    theta_point = theta_point, theta_bounds = theta_bounds[1, ],
    set_status = set_status, width = theta_bounds$width[1],
    coef_table = coef_table, beta1r = resid$beta1r,
    tau_star = ts$tau_star, tau_star_capped = ts$capped, tau_sweep = coarse,
    hetero_pvals = hetero_pvals, hetero_regime = suite_cfg$regime,
    significance_level = 0.05,
    relevance = relevance, theta_ols = unname(ols_coef[".y2"]),
    r2_w1 = resid$r2_w1, r2_y2 = resid$r2_y2,
    pc_var_explained = resid$pc_var_explained,
    news_loadings = resid$news_loadings, news_weights = resid$news_weights,
    used_maturities = resid$used_maturities, news_pc_scale = resid$news_pc_scale,
    n_obs = resid$n_obs, dates = resid$dates, tau_set = tau_set
  )
}
