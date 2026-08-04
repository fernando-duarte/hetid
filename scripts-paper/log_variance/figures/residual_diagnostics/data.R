# Standardized log-residual series at the Lewbel tau = 0 point, assembled for
# the residual-diagnostic figures. For each estimator the series is
#   xi_t = log(eps_t(b)^2) - x_t' theta_hat = log(eps_t(b)^2 / mu_hat_t),
# the realized squared residual measured against that estimator's OWN fitted
# conditional variance. Under conditionally normal eps the series is a draw from
# log chi^2_1 -- location included, because mu_hat is a fitted variance and not a
# free intercept, so the figures test the level of the fit and not only its
# shape. PPML and Harvey read the point fits already accepted upstream and are
# never refitted here; log-OLS recomputes its fixed projection, which is a
# deterministic transform of the same residual rather than an estimation.
# Definitions only; sourced by run.R.

paper_source_once(paper_path("log_variance", "core", "residual_map.R"))

LOGVAR_RESID_DIAG_ESTIMATORS <- c(
  ppml = "PPML",
  harvey = "Harvey",
  logols = "log-OLS"
)

# The one registry entry for an estimator id, by the same identity check the
# fitted-volatility driver uses; a miss or a tie is a hard error rather than a
# silently dropped series.
logvar_resid_diag_entry <- function(registry, estimator) {
  hit <- vapply(registry, function(entry) {
    identical(entry$estimator$metadata$estimator, estimator)
  }, logical(1))
  stopifnot(sum(hit) == 1L)
  registry[[which(hit)]]
}

# Accepted point-fit coefficients for one registry estimator. A missing or
# unaccepted point fit fails closed: the figure would otherwise silently drop an
# estimator and still render as though all three were compared.
logvar_resid_diag_point_coef <- function(registry, estimator, p) {
  entry <- logvar_resid_diag_entry(registry, estimator)
  fit <- entry$estimator$point_fit
  if (is.null(fit) || is.null(fit$coef)) {
    stop(sprintf(
      "residual diagnostics: %s has no accepted tau = 0 point fit", estimator
    ))
  }
  coef <- as.numeric(fit$coef)
  stopifnot(length(coef) == p, all(is.finite(coef)))
  coef
}

# theta_hat at b_point for all three estimators, in the display order of
# LOGVAR_RESID_DIAG_ESTIMATORS.
logvar_resid_diag_thetas <- function(registry, inputs, b_point) {
  x_mat <- logvar_design_matrix(inputs$pcr)
  p <- ncol(x_mat)
  proj <- logvar_projection(inputs$pcr)
  list(
    ppml = logvar_resid_diag_point_coef(registry, "ppml", p),
    harvey = logvar_resid_diag_point_coef(registry, "harvey", p),
    logols = logvar_theta_hat(b_point, inputs$w1, inputs$w2, proj)
  )
}

# The long frame the figures plot: one row per quarter per estimator, with the
# estimator column already a factor in display order so both figures key their
# colours the same way.
logvar_resid_diag_series <- function(inputs, b_point, thetas) {
  eps <- drop(inputs$w1 - inputs$w2 %*% b_point)
  if (any(eps == 0)) {
    stop("residual diagnostics: a residual is exactly zero at the tau = 0 point")
  }
  log_e2 <- 2 * log(abs(eps))
  x_mat <- logvar_design_matrix(inputs$pcr)
  keys <- names(LOGVAR_RESID_DIAG_ESTIMATORS)
  stopifnot(setequal(keys, names(thetas)))
  rows <- lapply(keys, function(key) {
    xi <- log_e2 - drop(x_mat %*% thetas[[key]])
    stopifnot(all(is.finite(xi)))
    data.frame(
      estimator = factor(
        LOGVAR_RESID_DIAG_ESTIMATORS[[key]],
        levels = unname(LOGVAR_RESID_DIAG_ESTIMATORS)
      ),
      qtr = inputs$qtr,
      xi = xi,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Summary the driver prints beside each render: the moments the figures compare
# against the log chi^2_1 reference, one row per estimator.
logvar_resid_diag_moments <- function(series) {
  ref <- digamma(0.5) + log(2)
  stats <- lapply(levels(series$estimator), function(nm) {
    xi <- series$xi[series$estimator == nm]
    centred <- xi - mean(xi)
    data.frame(
      estimator = nm,
      mean = mean(xi),
      variance = mean(centred^2),
      skewness = mean(centred^3) / mean(centred^2)^1.5,
      excess_kurtosis = mean(centred^4) / mean(centred^2)^2 - 3,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, stats)
  attr(out, "reference_mean") <- ref
  out
}
