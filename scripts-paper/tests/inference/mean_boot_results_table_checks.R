# Panel A rendering of the tau = 0 column mean_boot_results feeds: the bootstrap
# t statistic sits beneath a starred point estimate, never an interval. Sourced
# by mean_boot_results_checks.R, which owns the harness and the `check` binding,
# so the registered suite covers the renderer and its results layer together.

paper_source_once(paper_path(
  "mean_equation", "tables", "structural_table_parts.R"
))

stp_n_pc <- PAPER_ANALYSIS_CONTRACT$model$n_mean_pc
stp_coefs <- c(
  PAPER_ANALYSIS_CONTRACT$model$intercept_col,
  PAPER_ANALYSIS_CONTRACT$model$lag_expected_pc_cols,
  PAPER_ANALYSIS_CONTRACT$model$news_pc_cols
)
stp_design <- seq_len(1L + stp_n_pc)
stp_news <- 1L + stp_n_pc + seq_len(stp_n_pc)
# points chosen to span every star tier against a unit robust scale; one is
# unavailable in the full sample, and one is gated out by unstable boundedness
stp_point <- c(4, 0.5, 2.0, 1.7, NA_real_, 3.5, 1.75)
stp_ols <- c(0.3, -0.2, 0.1, 0.4, 0.2, -0.1, 0.5)
stp_obs <- 40L
stp_b <- 200L
stopifnot(
  length(stp_point) == length(stp_coefs),
  length(stp_ols) == length(stp_coefs)
)
set.seed(20260730L)
stp_dims <- list(NULL, stp_coefs)
stp_matrix <- function(values) {
  matrix(values, stp_b, length(stp_coefs), dimnames = stp_dims)
}
stp_draws <- stp_matrix(stats::rnorm(stp_b * length(stp_coefs)))
stp_draws <- sweep(stp_draws, 2, ifelse(is.na(stp_point), 0, stp_point), "+")
stp_status <- stp_matrix("bounded")
stp_status[seq_len(150L), 4L] <- "unreliable"
stp_point_t <- point_t_statistic(stp_point, stp_draws, stp_status)

# a regression whose coefficient names are exactly the contract's, so the OLS
# column's Newey-West helper and the row-order guard both run on real inputs
stp_reg <- as.data.frame(matrix(
  stats::rnorm(stp_obs * (length(stp_coefs) - 1L)),
  stp_obs,
  length(stp_coefs) - 1L,
  dimnames = list(NULL, stp_coefs[-1L])
))
stp_reg$y1 <- stats::rnorm(stp_obs)
stp_fit <- stats::lm(y1 ~ ., data = stp_reg)
stp_set <- list(
  beta1 = data.frame(
    coef = stp_coefs[stp_design], set_lower = -1, set_upper = 1,
    status = "bounded", lower_status = "bounded", upper_status = "bounded",
    stringsAsFactors = FALSE
  ),
  theta = data.frame(
    coef = stp_coefs[stp_news], set_lower = -2, set_upper = 2,
    status = "bounded", lower_status = "bounded", upper_status = "bounded",
    stringsAsFactors = FALSE
  )
)
stp_mean <- list(
  beta1_table = data.frame(
    coef = stp_coefs[stp_design], point = stp_point[stp_design],
    ols = stp_ols[stp_design], stringsAsFactors = FALSE
  ),
  theta_table = data.frame(
    coef = stp_coefs[stp_news], point = stp_point[stp_news],
    ols = stp_ols[stp_news], stringsAsFactors = FALSE
  ),
  set_tables = list(tau_005 = stp_set),
  tau_display = PAPER_ANALYSIS_CONTRACT$tau$baseline,
  ols_fit = stp_fit,
  sample = list(n = stp_obs)
)
stp_endpoints <- list(
  lower = stp_matrix(-abs(stats::rnorm(stp_b * length(stp_coefs)))),
  upper = stp_matrix(abs(stats::rnorm(stp_b * length(stp_coefs)))),
  lower_status = stp_matrix("bounded"),
  upper_status = stp_matrix("bounded")
)
stp_boot <- list(
  point_t = stp_point_t,
  inference = list(tau_005 = endpoint_target_table(
    stp_endpoints,
    rbind(stp_set$beta1, stp_set$theta)
  ))
)

stp_parts <- structural_equation_table_parts(stp_mean, stp_boot, stp_n_pc)
stp_rows <- seq_along(stp_coefs)
stp_estimates <- stp_parts$columns[[2L]][2L * stp_rows - 1L]
stp_statistics <- stp_parts$columns[[2L]][2L * stp_rows]
stp_reported <- stp_point_t$reason == "reported"
stp_star_p <- point_star_p(stp_point_t)
stp_stars <- sig_stars(stp_star_p)

check(
  "the tau = 0 sub-row prints a parenthesized statistic, not an interval",
  # a negative statistic sets its sign in math, so the minus arrives wrapped
  all(grepl(
    "^[(](?:[$]-[0-9]+[.][0-9]+[$]|[0-9]+[.][0-9]+)[)]$",
    stp_statistics[stp_reported]
  )) &&
    !any(grepl(",", stp_statistics, fixed = TRUE)) &&
    !any(grepl("infty", stp_statistics, fixed = TRUE))
)
check(
  "the printed statistic is the frame's, at the reporting statistic precision",
  identical(
    stp_statistics[stp_reported],
    sprintf(
      "(%s)",
      paper_math_negative(
        paper_format_number(
          stp_point_t$statistic[stp_reported],
          PAPER_REPORTING_CONTROL$cells$statistic_digits,
          "na"
        )
      )
    )
  )
)
check(
  "a star appears exactly when the basis p-value crosses a level",
  identical(
    endsWith(stp_estimates, "*"),
    !is.na(stp_star_p) & stp_star_p < paper_significance_level("one_star")
  ) &&
    length(unique(stp_stars)) >= 3L
)
# the default basis is the reported ratio's own tail, so the stars cannot order
# against the statistic beneath them; the bootstrap basis can, which is why it
# is not the default. Fails if the default flips or the column mapping is wrong.
check(
  "the default stars are the reported statistic's own two-sided normal tail",
  identical(PAPER_POINT_STAR_BASIS, "normal") &&
    identical(
      stp_stars,
      sig_stars(2 * stats::pnorm(-abs(stp_point_t$statistic)))
    )
)
stp_gated <- is.finite(stp_point_t$point) & !is.finite(stp_point_t$statistic)
stp_absent <- !is.finite(stp_point_t$point)
check(
  "a finite point whose statistic the gate withheld renders the missing token",
  any(stp_gated) && all(stp_statistics[stp_gated] == PAPER_NA_TOKEN)
)
check(
  "an unavailable full-sample point prints no statistic beneath it",
  any(stp_absent) && all(stp_statistics[stp_absent] == "")
)
check(
  "the tau > 0 sub-rows still print the shared table's pointwise interval",
  all(grepl(
    "^[$][(]-?[0-9.]+,",
    stp_parts$columns[[3L]][2L * stp_rows]
  ))
)
stp_reversed <- stp_boot
stp_reversed$point_t <- stp_point_t[rev(seq_len(nrow(stp_point_t))), ]
check(
  "a misaligned t-statistic frame fails the row-order guard",
  inherits(
    tryCatch(
      structural_equation_table_parts(stp_mean, stp_reversed, stp_n_pc),
      error = identity
    ),
    "error"
  )
)
