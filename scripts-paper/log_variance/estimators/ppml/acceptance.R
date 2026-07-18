# Post-fit machinery for the PPML log-variance response solve, split out of
# fit.R to keep both files under the hard 200-line cap. Defines
# the shared result skeleton and diagnostics container, the positive-response
# rank diagnostic, the per-rung acceptance check (fail-closed on any gate), the
# condition-recording glm.fit runner, and the accepted-fit assembler. Sourced
# by fit.R before logvar_ppml_fit_response; definitions only.

# Complete fit-result skeleton: every field the estimator-engine fit contract names is
# always present so success and every fail-closed branch share one shape.
logvar_ppml_result <- new_logvar_fit_result

# Slim diagnostics container with NA / empty defaults; callers override only the
# fields they can populate, keeping early fail-closed returns and accepted fits
# field-compatible for downstream consumers.
logvar_ppml_diag <- function(error_class, start_attempts, ...) {
  base <- list(
    warnings = character(0), messages = character(0),
    error_class = error_class, start_attempts = start_attempts,
    min_pos_response = NA_real_, rank_x_pos = NA_integer_,
    condition_weighted_scaled = NA_real_, rcond_info_raw = NA_real_,
    info_col_scale = NA_real_, score_norm_raw = NA_real_,
    score_norm_scaled = NA_real_
  )
  utils::modifyList(base, list(...))
}

# Positive-response rank diagnostic: scale each nonzero column of the positive
# rows by its Euclidean norm and count singular values above 1e-10 * d[1]. A
# zero column keeps its zero singular value and fails. Returns the rank count.
logvar_ppml_pos_rank <- function(
  y_scaled, x_mat, control = LOGVAR_PPML_CONTROL
) {
  x_pos <- x_mat[y_scaled > 0, , drop = FALSE]
  col_norms <- sqrt(colSums(x_pos^2))
  divisor <- ifelse(col_norms > 0, col_norms, 1)
  d <- svd(sweep(x_pos, 2, divisor, "/"))$d
  sum(d > control$rank_tol * d[1])
}

# Post-fit acceptance for one converged glm.fit rung on the scaled response.
# Returns the scaled coefficients, fitted means, score diagnostics, and the
# accept verdict; short-circuits with a reason on any failed gate so ill-posed
# fits never reach the score / conditioning computations.
logvar_ppml_accept <- function(
  fit, y_scaled, x_mat, control = LOGVAR_PPML_CONTROL
) {
  coef <- fit$coefficients
  bad <- function(reason) {
    list(
      accepted = FALSE, reason = reason,
      coef_scaled = coef
    )
  }
  if (any(!is.finite(coef))) {
    return(bad("nonfinite_coef"))
  }
  mu <- exp(drop(x_mat %*% coef))
  if (isTRUE(control$finite_mean_switch) &&
    (any(!is.finite(mu)) || any(mu <= 0))) {
    return(bad("nonpositive_mu"))
  }
  if (!isTRUE(fit$converged)) {
    return(bad("irls_not_converged"))
  }
  if (isTRUE(control$boundary_switch) && isTRUE(fit$boundary)) {
    return(bad("boundary"))
  }
  pos <- y_scaled > 0
  sc <- drop(crossprod(x_mat, y_scaled - mu))
  bound_unit <- max(1, stats::median(y_scaled[pos])) * colSums(abs(x_mat))
  score_norm <- max(abs(sc) / bound_unit)
  info_col_scale <- sqrt(colSums(mu * x_mat^2))
  if (any(!is.finite(info_col_scale)) || any(info_col_scale <= 0)) {
    return(bad("info_scale"))
  }
  rcond_scaled <- rcond(crossprod(sweep(sqrt(mu) * x_mat, 2, info_col_scale, "/")))
  reason <- NA_character_
  if (!(score_norm <= control$score_tol)) {
    reason <- "score_tolerance"
  } else if (!(rcond_scaled >= control$rcond_tol)) {
    reason <- "ill_conditioned"
  }
  list(
    accepted = is.na(reason), reason = reason, coef_scaled = coef, mu = mu,
    pos = pos, score_norm = score_norm, score_norm_raw = max(abs(sc)),
    info_col_scale = info_col_scale,
    condition_weighted_scaled = 1 / rcond_scaled,
    rcond_info_raw = rcond(crossprod(x_mat, mu * x_mat))
  )
}

# One glm.fit rung with the ratified control, recording (never silencing)
# warnings and messages and returning NULL fit on an IRLS error.
logvar_ppml_run <- function(
  cand, y_scaled, x_mat, control = LOGVAR_PPML_CONTROL
) {
  fit_function <- getExportedValue(
    "stats",
    control$fit_function
  )
  family_function <- getExportedValue(
    "stats",
    control$family
  )
  captured <- paper_capture_conditions(
    fit_function(
      x = x_mat, y = y_scaled,
      family = family_function(link = control$link), start = cand,
      control = stats::glm.control(
        maxit = control$glm_maxit,
        epsilon = control$glm_epsilon
      )
    )
  )
  error_warning <- if (is.na(captured$error_message)) {
    character(0)
  } else {
    captured$error_message
  }
  list(
    fit = captured$value,
    warnings = c(captured$warnings, error_warning),
    messages = captured$messages,
    error_class = captured$error_class,
    error_message = captured$error_message
  )
}

# Assemble the accepted-fit contract: recover the original-scale coefficients
# (log(response_scale) added to the intercept only), keep the raw scaled vector
# as warm_start, and report the scaled objective and score / conditioning.
logvar_ppml_success <- function(acc, run, y_scaled, response_scale,
                                attempts, rank_x_pos) {
  coef <- acc$coef_scaled
  coef[1] <- coef[1] + log(response_scale)
  objective <- sum(acc$mu) - sum(y_scaled[acc$pos] * log(acc$mu[acc$pos]))
  logvar_ppml_result(
    coef = coef, fit_status = LOGVAR_FIT_STATUS[["ok"]], converged = TRUE, objective = objective,
    score_norm = acc$score_norm, convergence_code = as.integer(run$fit$iter),
    warm_start = acc$coef_scaled,
    diagnostics = logvar_ppml_diag(
      NA_character_, attempts,
      warnings = run$warnings,
      messages = run$messages, min_pos_response = min(y_scaled[acc$pos]),
      rank_x_pos = rank_x_pos,
      condition_weighted_scaled = acc$condition_weighted_scaled,
      rcond_info_raw = acc$rcond_info_raw, info_col_scale = acc$info_col_scale,
      score_norm_raw = acc$score_norm_raw, score_norm_scaled = acc$score_norm
    )
  )
}
