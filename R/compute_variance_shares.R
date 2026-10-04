#' Variance Shares over the Mean Identified Set
#'
#' Computes expected-block, news-block, component and combined shares in percent
#' of the centered sample variance of the outcome. OLS treats news as exogenous;
#' the tau-zero column and slack-dependent ranges use the mean equation's
#' unconstrained reduced forms and joint identified sets.
#'
#' @param prepared List with exact named elements y, x, y2 and z. y is a finite
#'   real vector; x and y2 are finite real matrices with at least one column,
#'   and z is a finite real one-column matrix. All have the same observations
#'   in the same order, aligned by date upstream. Extra elements are ignored.
#'   Block column names must be unique across x and y2 and survive mean-fit
#'   name sanitization. x has no constant column and no column named y.
#'   Each block has positive column variances and absolute within-block sample
#'   correlations at most the configured tolerance. Cross-block covariance is
#'   unrestricted. No principal components are constructed or rows filtered.
#' @param control Named list extending \code{\link{VARIANCE_SHARE_CONTROL}}.
#' @return Plain list with rows, ols, point, set_cols, sets, news_row,
#'   combined_row, sd_c, n_obs and taus. The rows are expected_block, x names,
#'   news_block, y2 names and combined. set_cols contains lo, hi and status;
#'   sets contains theta and beta1 coefficient tables. Both use
#'   \code{sprintf("%.17g", taus)} keys in requested order. Block status is
#'   the least established theta status; finite diagnostics do not promote it.
#' @details Ranges use a full containing-box grid followed by local SLSQP
#'   searches over the joint feasible set. They are attained numerical ranges,
#'   without a global-extremum guarantee. Nonfinite enclosures yield missing
#'   block ranges; no admitted grid point or no accepted polished extreme is
#'   a structured error, not an emptiness conclusion. The combined share
#'   includes the cross-block covariance and need not equal the two block shares.
#'   The caller's RNG kind and present or absent seed are restored on success
#'   and error. The Box-Muller hidden-cache limitation of
#'   \code{\link{with_rng_scope}} applies.
#' @seealso \code{\link{compute_tau0_system}}, \code{\link{profile_mean_tau_path}}
#' @export
#'
#' @examples
#' with_rng_scope(
#'   {
#'     n <- 150L
#'     z <- matrix(rnorm(n), ncol = 1L, dimnames = list(NULL, "z"))
#'     x <- matrix(rnorm(n), ncol = 1L, dimnames = list(NULL, "expected"))
#'     y2 <- matrix(0.4 * x + sqrt(exp(0.4 + 0.8 * z)) * rnorm(n),
#'       ncol = 1L, dimnames = list(NULL, "news")
#'     )
#'     y <- drop(0.2 + 0.3 * x + 0.6 * y2 + rnorm(n))
#'     control <- VARIANCE_SHARE_CONTROL
#'     control$TAUS <- 0.05
#'     control$GRID_POINTS_PER_AXIS <- 11L
#'     shares <- compute_variance_shares(list(y = y, x = x, y2 = y2, z = z), control)
#'     shares$point
#'   },
#'   seed = 42,
#'   kind = c("Mersenne-Twister", "Inversion", "Rejection")
#' )
compute_variance_shares <- function(prepared, control = VARIANCE_SHARE_CONTROL) {
  validate_variance_share_inputs(prepared, control)
  with_rng_scope(
    {
      y <- prepared[["y"]]
      x <- prepared[["x"]]
      y2 <- prepared[["y2"]]
      n_e <- ncol(x)
      n_n <- ncol(y2)
      fit <- variance_share_fit(y, x, y2, prepared[["z"]], control)
      ols <- variance_share_ols(y, x, y2)
      covariance <- variance_share_covariances(y, x, y2, control)
      objectives <- variance_share_objectives(fit, colnames(x), covariance)
      news_row <- n_e + 2L
      combined_row <- n_e + n_n + 3L
      set_tables <- profile_mean_tau_path(fit, control$TAUS, control = control)
      e_rows <- match(colnames(x), set_tables[[1L]]$beta1$coef)
      assert_bad_argument_ok(
        !anyNA(e_rows) &&
          identical(set_tables[[1L]]$theta$coef, colnames(y2)),
        "Profile coefficient axes must match the supplied blocks",
        arg = "prepared"
      )
      set_cols <- lapply(set_tables, variance_share_set_column,
        e_rows = e_rows, objectives = objectives, covariance = covariance, control = control
      )
      for (cc in set_cols) {
        variance_share_assert_coherent(cc, list(c(1L, n_e), c(news_row, n_n)), control)
      }
      out <- list(
        rows = c("expected_block", colnames(x), "news_block", colnames(y2), "combined"),
        ols = variance_share_fixed(ols[colnames(x)], ols[colnames(y2)], covariance),
        point = variance_share_fixed(fit$beta1[colnames(x)], fit$point$theta, covariance),
        set_cols = set_cols,
        sets = lapply(set_tables, function(st) list(theta = st$theta, beta1 = st$beta1)),
        news_row = news_row,
        combined_row = combined_row,
        sd_c = sqrt(covariance$var_c),
        n_obs = length(y),
        taus = control$TAUS
      )
      assert_dimension_ok(length(out$ols) == combined_row, "Combined row must be last")
      if (!all(c(out$ols[combined_row], out$point[combined_row]) >= 0)) {
        stop_hetid("Combined share must be nonnegative.")
      }
      out
    },
    kind = c("Mersenne-Twister", "Inversion", "Rejection")
  )
}
