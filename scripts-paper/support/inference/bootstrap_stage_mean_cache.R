bootstrap_stage_mean_transport_gate <- function(
  collected, n_draws, failure_control
) {
  if (collected$n_failed > 0L) {
    cat("  failed draws by cause:\n")
    print(collected$failure_causes)
  }
  if (collected$n_failed >
    bootstrap_stage_failure_limit(n_draws, failure_control)) {
    stop(
      "endpoint bootstrap: ", collected$n_failed, " of ",
      n_draws, " draws failed"
    )
  }
  invisible(collected)
}

mean_boot_collection_validate <- function(
  collected, mean_spec, display_taus, n_draws, failure_control
) {
  fields <- c(
    "point_draws", "n_point_deficient", "endpoint_draws",
    "tau_star_draws", "n_capped", "n_failed", "failure_causes"
  )
  if (!bootstrap_stage_cache_exact_list(collected, fields)) {
    return("collection fields or class changed")
  }
  coefs <- mean_spec$coefs
  point <- collected$point_draws
  if (!bootstrap_stage_cache_matrix_ok(point, n_draws, coefs, "double") ||
    any(is.nan(point)) || any(is.infinite(point))) {
    return("point matrix changed")
  }
  point_missing <- rowSums(is.na(point))
  if (any(point_missing > 0L & point_missing < ncol(point))) {
    return("point row is partially missing")
  }
  all_na_point <- point_missing == ncol(point)
  endpoints <- collected$endpoint_draws
  if (!is.list(endpoints) || !is.null(attributes(endpoints)) ||
    length(endpoints) != length(display_taus)) {
    return("endpoint tau axis changed")
  }
  failed <- PAPER_ENDPOINT_STATUS[["failed"]]
  status_values <- unname(PAPER_ENDPOINT_STATUS)
  failed_mask <- NULL
  for (cell in endpoints) {
    if (!bootstrap_stage_cache_exact_list(
      cell, c("lower", "upper", "status")
    )) {
      return("endpoint cell fields changed")
    }
    valid <- c(
      bootstrap_stage_cache_matrix_ok(
        cell$lower, n_draws, coefs, "double"
      ),
      bootstrap_stage_cache_matrix_ok(
        cell$upper, n_draws, coefs, "double"
      ),
      bootstrap_stage_cache_matrix_ok(
        cell$status, n_draws, coefs, "character"
      )
    )
    if (!all(valid) ||
      !bootstrap_stage_cache_status_ok(cell$status, status_values)) {
      return("endpoint matrix changed")
    }
    bounded <- cell$status == PAPER_ENDPOINT_STATUS[["bounded"]]
    if (any(!is.finite(cell$lower[bounded])) ||
      any(!is.finite(cell$upper[bounded])) ||
      any(!is.na(cell$lower[!bounded])) ||
      any(!is.na(cell$upper[!bounded]))) {
      return("endpoint value/status mismatch")
    }
    cell_failed <- cell$status == failed
    row_failed <- apply(cell_failed, 1L, all)
    expected <- matrix(
      row_failed,
      nrow = n_draws, ncol = length(coefs)
    )
    if (!all(cell_failed == expected) ||
      (!is.null(failed_mask) && !identical(row_failed, failed_mask))) {
      return("mean failed-status mask changed")
    }
    failed_mask <- row_failed
  }
  tau_star <- collected$tau_star_draws
  if (!is.double(tau_star) || length(tau_star) != n_draws ||
    !is.null(attributes(tau_star)) || any(is.nan(tau_star)) ||
    any(is.infinite(tau_star))) {
    return("tau-star draws changed")
  }
  counts <- collected[c("n_point_deficient", "n_capped", "n_failed")]
  if (!all(vapply(
    counts, bootstrap_stage_cache_count_ok, logical(1),
    upper = n_draws
  ))) {
    return("mean count changed")
  }
  if (!identical(as.integer(sum(failed_mask)), collected$n_failed) ||
    !identical(is.na(tau_star), failed_mask)) {
    return("mean failure count changed")
  }
  if (any(failed_mask & !all_na_point)) {
    return("failed mean draw retains a point estimate")
  }
  causes <- collected$failure_causes
  causes_ok <- if (collected$n_failed == 0L) {
    is.null(causes)
  } else {
    bootstrap_stage_cache_table_ok(causes) &&
      length(dim(causes)) == 1L &&
      !is.null(names(causes)) &&
      all(nzchar(names(causes))) &&
      all(causes > 0L) &&
      identical(as.integer(sum(causes)), collected$n_failed)
  }
  if (!causes_ok) {
    return("mean failure causes changed")
  }
  if (!identical(
    as.integer(sum(all_na_point & !failed_mask)),
    collected$n_point_deficient
  )) {
    return("point-deficiency count changed")
  }
  tau_range <- range(mean_spec$tau_star_grid)
  present <- !is.na(tau_star)
  if (any(tau_star[present] < tau_range[[1L]]) ||
    any(tau_star[present] > tau_range[[2L]])) {
    return("tau-star range changed")
  }
  if (!identical(
    as.integer(sum(tau_star == tau_range[[2L]], na.rm = TRUE)),
    collected$n_capped
  )) {
    return("mean capped count changed")
  }
  if (collected$n_failed >
    bootstrap_stage_failure_limit(n_draws, failure_control)) {
    return("mean transport-failure gate exceeded")
  }
  TRUE
}
