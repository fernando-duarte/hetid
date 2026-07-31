# The mean-equation identified-set estimation, as a function of the beta2R
# specification so the pipeline can run it more than once on one sample. Split
# out of estimate_identified_set.R, which keeps the data build, the per-spec
# loop and the console report.
#
# The returned list carries `impose_null` so consumers read the specification
# from the object they were handed rather than from a global. With two
# specifications live, a global would silently describe whichever ran last.

paper_source_once(paper_path("support", "identification", "api.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("support", "identification", "tau_star.R"))
paper_source_once(paper_path("support", "identification", "identified_set_bootstrap.R"))
paper_source_once(paper_path("mean_equation", "inference", "refine_bounds_by_tau.R"))

estimate_mean_equation <- function(set_id_data, y1_col, x_cols, y2_cols, z_col,
                                   impose_null,
                                   tau_contract = PAPER_ANALYSIS_CONTRACT$tau) {
  tau_baseline <- tau_contract$baseline
  tau_cap <- tau_contract$cap
  tau_display <- tau_contract$display
  # reduced-form fits, de-meaned single instrument with unit weight, moments,
  # and the closed-form tau = 0 point, via the paper-owned shared estimator --
  # the endpoint bootstrap re-runs the identical recipe per draw, so the two
  # cannot drift apart
  sys_spec <- list(
    y1_col = y1_col, x_cols = x_cols, y2_cols = y2_cols, z_col = z_col,
    gamma = matrix(1, 1, length(y2_cols)), impose_null = impose_null
  )
  est <- estimate_set_id_system(set_id_data, sys_spec)
  beta1r <- est$beta1r
  w1 <- est$w1
  beta2r <- est$beta2r # I x p; rows = news PCs, cols match beta1r
  w2 <- est$w2
  z <- est$z
  moments <- est$moments
  gamma <- sys_spec$gamma
  point0 <- est$point0
  # critical slack tau*: the bounded -> unbounded transition of the joint set
  tau_sweep <- sweep_fixed_gamma(
    gamma, moments, seq(0, tau_cap, by = tau_contract$sweep_step), "coarse"
  )
  tau_star <- tau_star_fixed(gamma, moments, tau_sweep)
  # OLS benchmark on the identical sample (Y2 treated as exogenous)
  ols_fit <- stats::lm(
    stats::reformulate(c(x_cols, y2_cols), response = y1_col),
    data = set_id_data
  )
  # per-coefficient intervals of the joint identified set at each display
  # slack (theta profile bounds + beta1 functional bounds, the shared
  # coef_interval_tables recipe from support/identification/tau_star.R)
  set_tables <- lapply(
    tau_display, \(tau) coef_interval_tables(gamma, tau, moments, beta1r, beta2r)
  )
  names(set_tables) <- vapply(tau_display, paper_tau_key, character(1))
  # coef_interval_tables starts every profile solve at the origin and can settle
  # on a local vertex short of the true extreme, so the news intervals are
  # re-solved by the box multistart, walked up the display taus from a chain
  # seeded at the tau = 0 point. An endpoint moves only when a solve certifies a
  # feasible theta outside the origin-start interval, so this only ever adds
  # points the set provably contains -- it certifies feasibility, not global
  # optimality. These are the sound boxes: every set_tables consumer, and the
  # log-variance census reading them through mean_eq_bounds_tau, needs a box
  # that contains the set rather than one that clips it.
  refined <- set_id_display_tau_refinement_full(
    tau_display, if (is.null(point0)) NULL else point0$theta,
    gamma, moments, beta1r, beta2r
  )
  # both blocks take the refined set: theta from the widened box, beta1 from the
  # same certified points through its linear map. Refining only theta would
  # report the two over different sets under spec B.
  for (j in seq_along(set_tables)) {
    set_tables[[j]]$theta <- refined[[j]]$theta
    set_tables[[j]]$beta1 <- refined[[j]]$beta1
  }
  theta_table <- cbind(
    data.frame(
      coef = y2_cols,
      ols = unname(stats::coef(ols_fit)[y2_cols]),
      point = if (is.null(point0)) NA_real_ else point0$theta,
      row.names = NULL
    ),
    set_tables[[paper_tau_key(tau_baseline)]]$theta[
      c("set_lower", "set_upper", "status")
    ]
  )
  # design-coefficient recovery beta1(theta) = beta1R - beta2R' theta: point at
  # tau = 0, baseline-slack interval from the display tables
  beta1_point <- if (is.null(point0)) {
    stats::setNames(rep(NA_real_, length(beta1r)), names(beta1r))
  } else {
    hetid::recover_structural_coefficients(beta1r, beta2r, point0$theta)
  }
  beta1_table <- cbind(
    data.frame(
      coef = names(beta1r),
      ols = unname(stats::coef(ols_fit)[names(beta1r)]),
      point = unname(beta1_point),
      row.names = NULL
    ),
    set_tables[[paper_tau_key(tau_baseline)]]$beta1[
      c("set_lower", "set_upper", "status")
    ]
  )
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
  list(
    impose_null = impose_null,
    sample = list(n = nrow(set_id_data), span = range(set_id_data$qtr)),
    qtr = set_id_data$qtr,
    tau_baseline = tau_baseline,
    tau_display = tau_display,
    tau_contract = tau_contract,
    set_tables = set_tables,
    theta_table = theta_table,
    beta1_table = beta1_table,
    theta_point_cond = if (is.null(point0)) NA_real_ else point0$cond,
    tau_star = tau_star$tau_star,
    tau_star_capped = tau_star$capped,
    tau_sweep = tau_sweep,
    relevance = relevance,
    w2_cor = stats::cor(w2),
    # aligned system pieces, kept for the downstream heteroskedasticity tests
    # and the log-variance equation (w2 = y2 under the orthogonality null)
    w1 = w1, y1 = set_id_data[[y1_col]],
    y2 = as.matrix(set_id_data[y2_cols]), z = z, w2 = w2,
    # aligned estimation frame, column roles, and sweep cap, kept for the
    # endpoint bootstrap
    data = set_id_data, y1_col = y1_col, x_cols = x_cols, y2_cols = y2_cols,
    tau_cap = tau_cap,
    moments = moments,
    gamma = gamma,
    # reduced-form coefficients, kept for the bounds-by-tau figure
    beta1r = beta1r, beta2r = beta2r,
    ols_fit = ols_fit
  )
}
