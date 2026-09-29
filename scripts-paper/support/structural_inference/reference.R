# Helper function: the OLS reference column on the prepared samples
# the mean equation is OLS with the paper's Newey-West statistics and t tails;
# the variance equation is PPML on the squared OLS residuals of the variance
# rows, PC_R centred over that sample and not scaled, with HAC normal tails
structural_inference_reference <- function(prepared, settings) {
  stopifnot(identical(settings, prepared$settings))
  policy <- utils::modifyList(
    PAPER_REPORTING_CONTROL$mean_ols,
    list(hac_lags = settings$hac_lags)
  )
  data <- data.frame(prepared$y, prepared$x, prepared$y2, check.names = FALSE)
  names(data)[1L] <- settings$y
  ols <- stats::lm(stats::reformulate(c(settings$x, settings$y2), response = settings$y), data)
  estimate <- stats::coef(ols)
  if (anyNA(estimate)) stop("The OLS mean equation is rank deficient.", call. = FALSE)
  hac <- paper_newey_west_statistics(ols, estimate, names(estimate), policy)
  mean <- data.frame(
    panel = "mean", term = names(estimate), estimate = unname(estimate),
    se = unname(hac$se), statistic = unname(hac$statistic),
    p_value = unname(hac$p_value), reference_distribution = "t",
    df = stats::df.residual(ols), n_obs = stats::nobs(ols),
    r_squared = summary(ols)$r.squared, stringsAsFactors = FALSE
  )

  x_var <- scale(prepared$x_var[prepared$variance, , drop = FALSE],
    center = TRUE, scale = FALSE
  )
  ppml <- hetid::fit_log_variance(stats::residuals(ols)[prepared$variance]^2, x_var,
    estimator = "ppml"
  )
  if (!structural_inference_fit_ok(ppml)) {
    stop("The PPML variance fit did not converge: ", ppml$diagnostics$error_class,
      call. = FALSE
    )
  }
  errors <- hetid::compute_log_variance_se(ppml, hac_lags = settings$hac_lags)
  stopifnot(identical(errors$term, c("(Intercept)", settings$x_var)))
  statistic <- errors$coef / errors$hac
  variance <- data.frame(
    panel = "variance", term = errors$term, estimate = errors$coef, se = errors$hac,
    statistic = statistic, p_value = 2 * stats::pnorm(-abs(statistic)),
    reference_distribution = "normal", df = NA_real_, n_obs = nrow(x_var),
    r_squared = NA_real_, stringsAsFactors = FALSE
  )
  frame <- rbind(mean, variance)
  if (!all(is.finite(frame$statistic))) {
    stop("A reference standard error is zero or nonfinite.", call. = FALSE)
  }
  frame$available <- TRUE
  frame$reason <- "reported"
  rownames(frame) <- NULL
  list(
    frame = frame, ols = ols, ppml = ppml, variance_errors = errors,
    metadata = list(
      hac_lags = policy$hac_lags, prewhite = policy$prewhite, adjust = policy$adjust,
      variance_reference = "PPML on squared OLS residuals",
      variance_inference = "conditional on the fitted OLS residuals",
      mean_tail = "t with OLS residual degrees of freedom", variance_tail = "normal"
    )
  )
}
