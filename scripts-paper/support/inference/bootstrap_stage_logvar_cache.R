bootstrap_stage_anchor_gate <- function(anchor, stage_spec) {
  if (is.character(anchor)) stop(anchor, call. = FALSE)
  valid <- logvar_boot_anchor_validate(
    anchor, stage_spec$log_variance, stage_spec$tau$union
  )
  if (!isTRUE(valid)) {
    stop("vol set-endpoint bootstrap anchor: ", valid, call. = FALSE)
  }
  invisible(anchor)
}

bootstrap_stage_volatility_transport_gate <- function(
  n_failed, n_draws, failure_control
) {
  if (n_failed > bootstrap_stage_failure_limit(n_draws, failure_control)) {
    stop(
      "vol set-endpoint bootstrap: ", n_failed, " of ",
      n_draws, " draws failed"
    )
  }
  invisible(n_failed)
}

logvar_boot_collection_validate <- function(
  collected, n_failed, logvar_spec, taus, n_draws,
  failure_control, primary
) {
  estimator_ids <- logvar_spec$estimator_ids
  coefs <- logvar_spec$coefs
  if (!bootstrap_stage_cache_exact_list(collected, estimator_ids)) {
    return("estimator axis changed")
  }
  if (!bootstrap_stage_cache_count_ok(n_failed, n_draws)) {
    return("transport-failure count changed")
  }
  status_values <- unname(PAPER_ENDPOINT_STATUS)
  fully_failed <- rep(TRUE, n_draws)
  for (estimator_id in estimator_ids) {
    cells <- collected[[estimator_id]]
    if (!is.list(cells) || !is.null(attributes(cells)) ||
      length(cells) != length(taus)) {
      return("tau axis changed")
    }
    for (cell in cells) {
      fields <- c("lower", "upper", "lower_status", "upper_status")
      if (!bootstrap_stage_cache_exact_list(cell, fields)) {
        return("cell fields changed")
      }
      valid <- c(
        bootstrap_stage_cache_matrix_ok(
          cell$lower, n_draws, coefs, "double"
        ),
        bootstrap_stage_cache_matrix_ok(
          cell$upper, n_draws, coefs, "double"
        ),
        bootstrap_stage_cache_matrix_ok(
          cell$lower_status, n_draws, coefs, "character"
        ),
        bootstrap_stage_cache_matrix_ok(
          cell$upper_status, n_draws, coefs, "character"
        )
      )
      if (!all(valid) ||
        !bootstrap_stage_cache_status_ok(
          cell$lower_status, status_values
        ) ||
        !bootstrap_stage_cache_status_ok(
          cell$upper_status, status_values
        )) {
        return("cell matrix changed")
      }
      values_ok <- bootstrap_stage_cache_side_values_ok(
        cell$lower, cell$lower_status, "lower"
      ) && bootstrap_stage_cache_side_values_ok(
        cell$upper, cell$upper_status, "upper"
      )
      if (!values_ok || !bootstrap_stage_cache_interval_order_ok(
        cell$lower, cell$upper,
        cell$lower_status, cell$upper_status
      )) {
        return("endpoint value/status mismatch")
      }
      masks <- bootstrap_stage_cache_failure_masks(
        cell$lower, cell$upper,
        cell$lower_status, cell$upper_status
      )
      if (any(masks$any & !masks$full)) {
        return("partial failed endpoint cell")
      }
      fully_failed <- fully_failed & masks$full
    }
  }
  if (n_failed > sum(fully_failed)) {
    return("transport failures exceed fully failed rows")
  }
  if (isTRUE(primary) && n_failed >
    bootstrap_stage_failure_limit(n_draws, failure_control)) {
    return("primary transport-failure gate exceeded")
  }
  cells <- logvar_boot_failure_cells(collected, estimator_ids)
  if (logvar_boot_failure_bad_count(cells, failure_control) > 0L) {
    return("structured-status gate exceeded")
  }
  TRUE
}

logvar_boot_anchor_validate <- function(anchor, logvar_spec, taus) {
  estimator_ids <- logvar_spec$estimator_ids
  coefs <- logvar_spec$coefs
  if (!bootstrap_stage_cache_exact_list(anchor, estimator_ids)) {
    return("estimator axis changed")
  }
  status_values <- unname(PAPER_ENDPOINT_STATUS)
  for (estimator_id in estimator_ids) {
    cells <- anchor[[estimator_id]]
    if (!is.list(cells) || length(cells) != length(taus)) {
      return("tau axis changed")
    }
    live <- FALSE
    for (cell in cells) {
      if (!bootstrap_stage_cache_exact_list(
        cell, c("lower", "upper", "lower_status", "upper_status")
      )) {
        return("cell fields changed")
      }
      endpoints_ok <- is.double(cell$lower) && is.double(cell$upper) &&
        length(cell$lower) == length(coefs) &&
        length(cell$upper) == length(coefs) &&
        is.null(attributes(cell$lower)) &&
        is.null(attributes(cell$upper))
      statuses_ok <- is.character(cell$lower_status) &&
        is.character(cell$upper_status) &&
        length(cell$lower_status) == length(coefs) &&
        length(cell$upper_status) == length(coefs) &&
        is.null(attributes(cell$lower_status)) &&
        is.null(attributes(cell$upper_status)) &&
        bootstrap_stage_cache_status_ok(
          cell$lower_status, status_values
        ) &&
        bootstrap_stage_cache_status_ok(
          cell$upper_status, status_values
        )
      if (!endpoints_ok || !statuses_ok) {
        return("cell vector changed")
      }
      values_ok <- bootstrap_stage_cache_side_values_ok(
        cell$lower, cell$lower_status, "lower"
      ) && bootstrap_stage_cache_side_values_ok(
        cell$upper, cell$upper_status, "upper"
      )
      if (!values_ok) {
        return("endpoint value/status mismatch")
      }
      live <- live || any(c(
        cell$lower_status, cell$upper_status
      ) != PAPER_ENDPOINT_STATUS[["failed"]])
    }
    if (!live) {
      return(paste0("estimator is entirely failed: ", estimator_id))
    }
  }
  TRUE
}
