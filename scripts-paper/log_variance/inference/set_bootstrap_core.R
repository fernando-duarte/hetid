logvar_box_seed <- function(box) {
  midpoint <- (box$set_lower + box$set_upper) / 2
  ifelse(is.finite(midpoint), midpoint, 0)
}

logvar_run_estimator <- function(
  est_obj, spec, boxes, qss, b_point, taus = spec$taus
) {
  lapply(seq_along(taus), function(index) {
    seed <- if (is.null(b_point)) logvar_box_seed(boxes[[index]]) else b_point
    schema <- if (is.null(est_obj)) {
      NULL
    } else {
      tryCatch(
        logvar_engine_set_at_tau(
          est_obj, qss[[index]], boxes[[index]],
          b_seed = seed,
          max_grid_points = spec$grid_cap, max_fit_evals = spec$fit_budget,
          cold_start_check = FALSE, tau = taus[[index]]
        )$schema,
        error = function(error) NULL
      )
    }
    logvar_side_record(schema, spec$coefs)
  })
}

logvar_side_record <- function(schema, coefs) {
  if (is.null(schema) || !identical(schema$coef, coefs)) {
    n_coef <- length(coefs)
    return(list(
      lower = rep(NA_real_, n_coef),
      upper = rep(NA_real_, n_coef),
      lower_status = rep(PAPER_ENDPOINT_STATUS[["failed"]], n_coef),
      upper_status = rep(PAPER_ENDPOINT_STATUS[["failed"]], n_coef)
    ))
  }
  list(
    lower = schema$lower,
    upper = schema$upper,
    lower_status = schema$lower_status,
    upper_status = schema$upper_status
  )
}

logvar_set_boot_collect <- function(raw, spec) {
  failed_estimator <- lapply(spec$taus, function(...) logvar_side_record(NULL, spec$coefs))
  raw <- lapply(raw, function(draw) {
    if (!is.character(draw)) {
      return(draw)
    }
    stats::setNames(rep(list(failed_estimator), length(spec$estimator_ids)), spec$estimator_ids)
  })
  stack <- function(estimator, tau_index, field) {
    out <- do.call(rbind, lapply(raw, function(draw) {
      draw[[estimator]][[tau_index]][[field]]
    }))
    colnames(out) <- spec$coefs
    out
  }
  stats::setNames(lapply(spec$estimator_ids, function(estimator) {
    lapply(seq_along(spec$taus), function(tau_index) {
      list(
        lower = stack(estimator, tau_index, "lower"),
        upper = stack(estimator, tau_index, "upper"),
        lower_status = stack(estimator, tau_index, "lower_status"),
        upper_status = stack(estimator, tau_index, "upper_status")
      )
    })
  }), spec$estimator_ids)
}
