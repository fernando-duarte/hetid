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

  # OLS benchmarks: the structural equation under several specifications that
  # vary (a) whether the SDF-news PC Y2 enters and (b) the number of consumption
  # lags (0, 1, or the baseline 4). All on the SAME aligned sample (nested
  # regressors). Significance stars use Newey-West HAC standard errors (the
  # time-series standard for a quarterly regression).
  # Newey-West HAC lag via the standard floor(4 (T/100)^(2/9)) rule (fixed across
  # specs so it is reportable in the caption).
  nw_lag <- floor(4 * (length(w1) / 100)^(2 / 9))
  hac_p <- function(fit) {
    ct <- lmtest::coeftest(
      fit,
      vcov. = sandwich::NeweyWest(fit, lag = nw_lag, prewhite = FALSE)
    )
    stats::setNames(ct[, "Pr(>|t|)"], rownames(ct))
  }
  lag_cols <- c("l.y1", "l2.y1", "l3.y1", "l4.y1")
  make_ols_spec <- function(label, with_y2, lags) {
    preds <- paste0("pc", 1:4)
    if (lags >= 1L) preds <- c(preds, lag_cols[seq_len(lags)])
    df <- data.frame(.y1 = resid$y1_level, resid$design_aligned[, preds, drop = FALSE])
    if (with_y2) df$.y2 <- resid$y2
    fit <- lm(.y1 ~ ., data = df)
    list(
      label = label, with_y2 = with_y2, lags = lags,
      coef = coef(fit), p = hac_p(fit), r2 = summary(fit)$r.squared
    )
  }
  ols_specs <- list(
    make_ols_spec("with $Y_2$, 0 lags", TRUE, 0L),
    make_ols_spec("no $Y_2$, 0 lags", FALSE, 0L),
    make_ols_spec("with $Y_2$, 1 lag", TRUE, 1L),
    make_ols_spec("no $Y_2$, 1 lag", FALSE, 1L),
    make_ols_spec("with $Y_2$, 4 lags", TRUE, 4L),
    make_ols_spec("no $Y_2$, 4 lags", FALSE, 4L)
  )
  # The 4-lag specs feed the per-coefficient table and the round-trip identity.
  s_incl <- ols_specs[[5]]
  s_excl <- ols_specs[[6]]
  ols_incl <- s_incl$coef
  ols_incl_p <- s_incl$p
  ols_excl <- s_excl$coef
  ols_excl_p <- s_excl$p
  ols_r2 <- c(incl_news = s_incl$r2, excl_news = s_excl$r2)

  # beta1(theta) point recovery (tau = 0); kept for the round-trip identity.
  beta2r <- resid$beta2r
  beta2r_mat <- matrix(beta2r, nrow = 1, dimnames = list("news_pc", names(beta2r)))
  beta1_point <- if (is.null(pt0)) {
    stats::setNames(rep(NA_real_, length(resid$beta1r)), names(resid$beta1r))
  } else {
    recover_structural_coefficients(resid$beta1r, beta2r_mat, theta_point)
  }

  # Set ID at two slacks: tau = tau_set (0.05) and tau = 0.5.
  tau_set2 <- 0.5
  qs_set50 <- build_pipeline_quadratic_system(gamma, tau_set2, moments)
  theta_bounds50 <- solve_all_profile_bounds(qs_set50$quadratic)
  # beta1_p interval over the set defined by `quad`: [beta1r - fmax, beta1r - fmin].
  beta1_interval <- function(quad, p) {
    cp <- unname(beta2r[p])
    fmin <- solve_linear_functional_bound(quad, cp, "min")
    fmax <- solve_linear_functional_bound(quad, cp, "max")
    list(
      lower = unname(resid$beta1r[p]) - fmax$bound,
      upper = unname(resid$beta1r[p]) - fmin$bound,
      bounded = isTRUE(fmin$bounded) && isTRUE(fmax$bounded),
      valid = isTRUE(fmin$valid) && isTRUE(fmax$valid)
    )
  }
  theta_interval <- function(b) {
    list(
      lower = b$lower[1], upper = b$upper[1],
      bounded = isTRUE(b$bounded_lower[1]) && isTRUE(b$bounded_upper[1]),
      valid = isTRUE(b$valid_lower[1]) && isTRUE(b$valid_upper[1])
    )
  }

  # Coefficient table: two OLS columns (incl./excl. SDF news, with HAC p-values
  # for stars), the recovered point (tau = 0, kept for the identity), and the
  # identified-set intervals at tau = 0.05 and tau = 0.5.
  coef_row <- function(coef, oi, oip, oe, oep, point, s05, s50) {
    data.frame(
      coef = coef,
      ols = unname(oi), ols_p = unname(oip),
      ols_no = unname(oe), ols_no_p = unname(oep),
      point = unname(point),
      set_lower = s05$lower, set_upper = s05$upper,
      bounded = isTRUE(s05$bounded), valid = isTRUE(s05$valid),
      set50_lower = s50$lower, set50_upper = s50$upper,
      set50_bounded = isTRUE(s50$bounded), set50_valid = isTRUE(s50$valid),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }
  design_rows <- lapply(names(resid$beta1r), function(p) {
    coef_row(
      p, ols_incl[p], ols_incl_p[p], ols_excl[p], ols_excl_p[p], beta1_point[p],
      beta1_interval(qs_set$quadratic, p), beta1_interval(qs_set50$quadratic, p)
    )
  })
  theta_row <- coef_row(
    "theta", ols_incl[".y2"], ols_incl_p[".y2"], NA_real_, NA_real_, theta_point,
    theta_interval(theta_bounds), theta_interval(theta_bounds50)
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
    relevance = relevance, theta_ols = unname(ols_incl[".y2"]),
    ols_r2 = ols_r2, ols_specs = ols_specs, nw_lag = nw_lag, tau_set2 = tau_set2,
    r2_w1 = resid$r2_w1, r2_y2 = resid$r2_y2,
    pc_var_explained = resid$pc_var_explained,
    news_loadings = resid$news_loadings, news_weights = resid$news_weights,
    used_maturities = resid$used_maturities, news_pc_scale = resid$news_pc_scale,
    n_obs = resid$n_obs, dates = resid$dates, tau_set = tau_set
  )
}
