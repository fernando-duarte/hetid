# Fit candidates in their original order, carrying only successful warm starts.
# The legacy path keeps its one-shot seam; retained samples can supply a prepared fitter
fit_over_candidates <- function(candidates, box, x_var, estimator,
                                fitter = NULL, retain = FALSE) {
  rows <- vector("list", nrow(candidates))
  records <- if (retain) vector("list", nrow(candidates)) else NULL
  warm <- NULL
  n_failed <- 0L
  for (i in seq_len(nrow(candidates))) {
    theta <- candidates[i, ]
    names(theta) <- colnames(candidates)
    fit <- if (is.null(fitter)) {
      fit_log_variance_at_b(theta, box$w1, box$w2, x_var,
        estimator = estimator, start = warm
      )
    } else {
      fitter(theta, warm)
    }
    if (retain) records[[i]] <- fit
    if (log_variance_fit_ok(fit)) {
      rows[[i]] <- fit$coef
      warm <- fit$warm_start
    } else {
      n_failed <- n_failed + 1L
    }
  }
  rows <- rows[!vapply(rows, is.null, logical(1))]
  out <- list(
    coefs = if (length(rows) == 0L) NULL else do.call(rbind, rows),
    n_failed = n_failed
  )
  if (retain) out$fits <- records
  out
}

# Preserve candidate identity when a fit fails, without repeating responses and designs
log_variance_sample_records <- function(fits, candidates, labels) {
  coef_matrix <- matrix(NA_real_, nrow(candidates), length(labels),
    dimnames = list(rownames(candidates), labels)
  )
  records <- vector("list", length(fits))
  for (i in seq_along(fits)) {
    fit <- fits[[i]]
    if (log_variance_fit_ok(fit)) coef_matrix[i, ] <- fit$coef
    records[[i]] <- fit[c(
      "fit_status", "converged", "objective", "score_norm",
      "convergence_code", "diagnostics"
    )]
  }
  names(records) <- rownames(candidates)
  list(coefficients = coef_matrix, fits = records)
}
