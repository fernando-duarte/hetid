# Schema checks for the estimator-generic set engine: the fifteen contract
# columns, the result fields, tau plus the evaluation counts, and the exact-
# zero-residual fail-closed path. Sourced by test_engine.R after
# oracle_checks.R, whose orc_make_fixture / orc_run_pair
# helpers and orc_safe result this file reuses.

set.seed(23)

# schema completeness: fifteen contract columns, the result fields, and tau
# plus the evaluation counts somewhere in the returned object
orc_schema_cols <- c(
  "coef", "lower", "upper", "lower_status", "upper_status", "arg_lower",
  "arg_upper", "lower_fit_status", "upper_fit_status", "fit_failure_count",
  "lower_constraint_residual", "upper_constraint_residual", "estimator",
  "target_functional", "sample_id"
)
orc_present <- unique(c(
  names(orc_safe$engine), names(orc_safe$engine$schema),
  names(orc_safe$engine$diagnostics)
))
check(
  "engine schema carries all fifteen contract columns",
  all(orc_schema_cols %in% names(orc_safe$engine$schema))
)
check(
  "engine result carries table, schema, domain_info, and diagnostics",
  all(c(
    "table", "schema", "n_cross", "n_feasible", "domain_info",
    "diagnostics"
  ) %in% names(orc_safe$engine))
)
check(
  "engine reports tau and the evaluation counts somewhere in the result",
  all(c("tau", "n_attempted", "n_evaluated", "n_cached", "n_failed") %in%
    orc_present)
)
# tau = 0 exact-zero residual: the seed sets eps[1] exactly zero (w1[1] =
# w2[1, ] %*% b_seed), so the map value is nonfinite and the diverging side
# fails closed with no fabricated finite endpoint
orc_w2_zero <- matrix(runif(orc_n * 2L, -0.25, 0.25), orc_n, 2L)
orc_w2_zero[1, ] <- c(1, 0)
orc_seed_zero <- c(0.3, 0)
orc_w1_zero <- 5 + runif(orc_n)
orc_w1_zero[1] <- drop(orc_w2_zero[1, ] %*% orc_seed_zero)
orc_fx_zero <- orc_make_fixture(orc_w2_zero, orc_w1_zero)
orc_zero <- orc_run_pair(orc_fx_zero, orc_seed_zero)
orc_iz <- which(orc_zero$engine$schema$coef == "(Intercept)")
orc_map_zero <- logvar_theta_hat(
  orc_seed_zero, orc_fx_zero$w1, orc_fx_zero$w2,
  orc_fx_zero$proj
)
check(
  "tau = 0 exact-zero residual: the seed map value is nonfinite",
  !is.finite(orc_map_zero[orc_iz])
)
check(
  "tau = 0 exact-zero residual: engine fails closed with no fabricated endpoint",
  orc_same_table(orc_zero) &&
    orc_zero$engine$schema$lower_status[orc_iz] != "bounded" &&
    !is.finite(orc_zero$engine$table$set_lower[orc_iz])
)
bad_fit_status <- try(
  new_logvar_fit_result(
    numeric(0), "typo", FALSE, NA_real_, NA_real_,
    NA_integer_, NULL, list()
  ),
  silent = TRUE
)
stopifnot(
  inherits(bad_fit_status, "try-error"),
  identical(
    paper_endpoint_status_reduce(
      c(
        PAPER_ENDPOINT_STATUS[["bounded"]],
        PAPER_ENDPOINT_STATUS[["unbounded"]]
      ),
      c(
        PAPER_ENDPOINT_STATUS[["unreliable"]],
        PAPER_ENDPOINT_STATUS[["bounded"]]
      )
    ),
    c(
      PAPER_ENDPOINT_STATUS[["unreliable"]],
      PAPER_ENDPOINT_STATUS[["unbounded"]]
    )
  ),
  inherits(
    try(paper_endpoint_status_worst("typo"), silent = TRUE),
    "try-error"
  )
)
