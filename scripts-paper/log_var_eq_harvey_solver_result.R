# Result skeleton, recession-classification map, and the stability-pair
# evaluation for the Harvey variance solver, split from the acceptance
# module for the repository line cap. Definitions only; sourced by
# log_var_eq_harvey_solver.R beside the acceptance helpers.

# Complete fit-result skeleton: every Plan 7 field is always present so success
# and each fail-closed branch share one shape.
hv_result <- function(coef = NULL, fit_status, converged = FALSE,
                      objective = NA_real_, score_norm = NA_real_,
                      convergence_code = -1L, warm_start = NULL,
                      error_class = NA_character_, n_zero = 0L,
                      rank_x_pos = NA_integer_, rcond_info = NA_real_,
                      n_halvings = NA_integer_, start_attempts = list(),
                      per_start_criteria = NULL, recession = NULL,
                      info_matrix = NULL) {
  list(
    coef = coef, fit_status = fit_status, converged = converged,
    objective = objective, score_norm = score_norm,
    convergence_code = convergence_code,
    diagnostics = list(
      warnings = character(0), messages = character(0), error_class = error_class,
      start_attempts = start_attempts, n_zero_response = n_zero,
      rank_x_pos = rank_x_pos, rcond_info = rcond_info, n_halvings = n_halvings,
      per_start_criteria = per_start_criteria, recession_certificate = recession,
      info_matrix = info_matrix
    ),
    warm_start = warm_start
  )
}

# Map a nonpassing recession classification to its fit status and error class.
hv_recession_map <- list(
  negative_recession = c("nonexistence", "negative_recession"),
  zero_recession = c("nonconvergence", "zero_recession_unresolved"),
  certificate_failure = c("nonconvergence", "recession_certificate_failed")
)

# Evaluate one stability pair without fitting: validate the response and start,
# require finite eta and strictly positive variance, a finite safe ratio, score,
# and information, then take one backtracked scoring proposal (the first trial).
hv_precheck_pair <- function(pair, label, x_mat, n, p, col_abs, chol_xx) {
  rec <- function(ok, reason, objective = NA_real_, score_norm = NA_real_,
                  proposal_ok = FALSE) {
    list(
      label = label, ok = ok, objective = objective, score_norm = score_norm,
      proposal_ok = proposal_ok, reason = reason
    )
  }
  y <- if (!is.null(pair$response)) pair$response else pair$y
  start <- pair$start
  if (!is.numeric(y) || length(y) != n || anyNA(y) || any(!is.finite(y)) ||
    any(y < 0)) {
    return(rec(FALSE, "invalid_response"))
  }
  if (!is.numeric(start) || length(start) != p || !all(is.finite(start))) {
    return(rec(FALSE, "invalid_start"))
  }
  pos <- y > 0
  cur <- hv_eval(start, y, x_mat, pos, col_abs)
  if (is.null(cur)) {
    return(rec(FALSE, "nonfinite_start_eval"))
  }
  mu <- exp(cur$eta)
  if (!all(is.finite(mu)) || !all(mu > 0)) {
    return(rec(FALSE, "nonpositive_mu", cur$q, cur$score_norm))
  }
  info <- 0.5 * crossprod(x_mat, cur$r * x_mat)
  if (!all(is.finite(info))) {
    return(rec(FALSE, "nonfinite_info", cur$q, cur$score_norm))
  }
  dir <- hv_chol_solve(chol_xx, cur$moment)
  trial <- hv_eval(start + dir, y, x_mat, pos, col_abs)
  proposal_ok <- !is.null(trial)
  rec(
    proposal_ok, if (proposal_ok) NA_character_ else "proposal_nonfinite",
    cur$q, cur$score_norm, proposal_ok
  )
}
