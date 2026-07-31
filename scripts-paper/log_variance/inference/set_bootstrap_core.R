logvar_box_seed <- function(box) {
  midpoint <- (box$set_lower + box$set_upper) / 2
  ifelse(is.finite(midpoint), midpoint, 0)
}

logvar_run_estimator <- function(
  est_obj, spec, boxes, qss, b_point, taus = spec$taus
) {
  tau0 <- bootstrap_stage_logvar_tau0_slot(taus)
  lapply(seq_along(taus), function(index) {
    if (identical(index, tau0)) {
      return(logvar_point_record(est_obj, b_point, spec$coefs))
    }
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

# At tau = 0 the news set is the single point b_point, so the volatility
# coefficient is one direct evaluation there instead of a min/max search over
# an interval. `point` is the authoritative field. An absent estimator context
# or a coefficient axis that disagrees with the spec is the same wholesale
# estimator failure the searched slots report as "failed"; a missing point (a
# rank-deficient tau = 0 system), a rejected fit, or a nonfinite coefficient is
# "unreliable". "unbounded" is impossible for a point and is never emitted.
logvar_point_record <- function(est_obj, b_point, coefs) {
  if (!is.list(est_obj) || !identical(est_obj$coef_labels, coefs)) {
    return(logvar_failed_record(coefs, point = TRUE))
  }
  # Read the point the way this estimator's own full-sample driver publishes it,
  # so the t statistic's numerator and the scale of its draws come from one
  # estimator. The two differ: a context exposing point_fit already solved at
  # b_point from its start plan and its driver reads that field
  # (harvey/run_sets.R), while a context without it publishes fit_at_b's own
  # start ladder (ppml/run_sets.R). Calling fit_at_b for both would silently
  # re-solve Harvey under a different ladder, which can accept where the
  # published recipe rejected.
  fit <- if (is.null(b_point)) {
    NULL
  } else if ("point_fit" %in% names(est_obj)) {
    est_obj$point_fit
  } else {
    tryCatch(est_obj$fit_at_b(b_point), error = function(error) NULL)
  }
  # identity of the axis, not just its length: the collector stacks by position
  # and relabels afterwards, so a same-length permutation would attach the right
  # names to the wrong columns and no downstream check could see it -- the names
  # are gone by then. The searched slots already enforce this in logvar_side_record.
  ok <- logvar_fit_ok(fit) && identical(names(fit$coef), coefs)
  point <- if (ok) unname(fit$coef) else rep(NA_real_, length(coefs))
  status <- rep(
    PAPER_ENDPOINT_STATUS[[if (ok) "bounded" else "unreliable"]],
    length(coefs)
  )
  logvar_point_mirrors(point, status)
}

# Compatibility mirrors, written only after the direct evaluation and only as
# exact copies: logvar_boot_failure_gate pools lower_status and upper_status to
# count failures, and copying keeps it working unchanged. They are never an
# inference input, and no side, midpoint or tolerance is ever chosen here.
logvar_point_mirrors <- function(point, point_status) {
  list(
    lower = point,
    upper = point,
    lower_status = point_status,
    upper_status = point_status,
    point = point,
    point_status = point_status
  )
}

logvar_failed_record <- function(coefs, point = FALSE) {
  n_coef <- length(coefs)
  values <- rep(NA_real_, n_coef)
  status <- rep(PAPER_ENDPOINT_STATUS[["failed"]], n_coef)
  if (isTRUE(point)) {
    return(logvar_point_mirrors(values, status))
  }
  list(
    lower = values,
    upper = values,
    lower_status = status,
    upper_status = status
  )
}

logvar_side_record <- function(schema, coefs) {
  if (is.null(schema) || !identical(schema$coef, coefs)) {
    return(logvar_failed_record(coefs))
  }
  list(
    lower = schema$lower,
    upper = schema$upper,
    lower_status = schema$lower_status,
    upper_status = schema$upper_status
  )
}

logvar_set_boot_collect <- function(raw, spec) {
  tau0 <- bootstrap_stage_logvar_tau0_slot(spec$taus)
  failed_estimator <- lapply(seq_along(spec$taus), function(index) {
    logvar_failed_record(spec$coefs, point = identical(index, tau0))
  })
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
      fields <- bootstrap_stage_logvar_cell_fields(tau_index, spec$taus)
      stats::setNames(lapply(fields, function(field) {
        stack(estimator, tau_index, field)
      }), fields)
    })
  }), spec$estimator_ids)
}
