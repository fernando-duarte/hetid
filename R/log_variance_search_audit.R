lv_set_audit_run <- function(estimator, path, theta_tables, taus, seed, grid_cap,
                             fit_budget, grid_selector = NULL,
                             control = log_variance_search_control()) {
  keys <- profile_tau_key(taus)
  lv_set_assert(length(taus) > 0L, !anyDuplicated(keys), all(keys %in% names(theta_tables)))
  cache <- new.env(parent = emptyenv())
  stats::setNames(lapply(seq_along(taus), function(i) {
    tryCatch(
      list(ok = TRUE, result = lv_set_search(estimator,
        lv_set_quadratic(path, taus[[i]]), theta_tables[[keys[[i]]]],
        seed = seed,
        max_grid_points = grid_cap, cache = cache, budget = lv_set_budget(fit_budget),
        starts_per_side = control$search$audit_starts_per_side,
        grid_selector = grid_selector, tau = taus[[i]], control = control
      )),
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
  }), keys)
}

lv_set_audit_selector <- function(audit, selector_id) {
  for (key in names(audit)) {
    entry <- audit[[key]]
    n_raw <- entry$result$diagnostics$n_raw_feasible
    if (!isTRUE(entry$ok) || (!is.null(n_raw) && (is.na(n_raw) || n_raw == 0L))) {
      next
    }
    ran <- entry$result$diagnostics$selector$selector_id
    if (!identical(ran, selector_id)) {
      lv_set_stop(
        "The audit at tau ", key, " chose its grid with ",
        if (is.null(ran)) "no selector" else ran, ", not with ", selector_id, "."
      )
    }
  }
  invisible(selector_id)
}

lv_set_audit_side <- function(schema, side, j) {
  list(
    value = schema[[side]][[j]], status = schema[[paste0(side, "_status")]][[j]],
    source = schema[[paste0(side, "_source")]][[j]],
    arg = schema[[paste0("arg_", side)]][[j]],
    residual = schema[[paste0(side, "_constraint_residual")]][[j]]
  )
}

lv_set_audit_extreme <- function(side, primary, audit) {
  candidates <- list()
  if (identical(primary$status, "bounded") && is.finite(primary$value)) {
    candidates$primary <- primary
  }
  if (!is.null(audit) && identical(audit$status, "bounded") && is.finite(audit$value)) {
    candidates$audit <- audit
  }
  if (!length(candidates)) {
    return(c(primary, list(origin = "primary")))
  }
  values <- vapply(candidates, function(candidate) candidate$value, numeric(1))
  origin <- names(candidates)[[if (side == "lower") which.min(values) else which.max(values)]]
  c(candidates[[origin]], list(origin = origin))
}

lv_set_audit_apply <- function(primary, audit, control = log_variance_search_control(),
                               selector_id = NULL) {
  provenance <- lv_set_selector_provenance(audit, selector_id)
  if (!is.null(selector_id)) lv_set_audit_selector(audit, selector_id)
  # warn on failed audits because their cause may be code rather than data
  for (key in names(primary)) {
    if (!is.null(audit[[key]]) && !isTRUE(audit[[key]]$ok)) {
      warning("The audit at tau ", key, " stopped, its sides are unreliable: ",
        audit[[key]]$error,
        call. = FALSE
      )
    }
  }
  tolerance <- control$search$endpoint_agreement_rtol
  results <- list()
  rows <- list()
  for (key in names(primary)) {
    schema <- primary[[key]]$schema
    entry <- audit[[key]]
    failed <- is.null(entry) || !isTRUE(entry$ok)
    for (j in seq_len(nrow(schema))) {
      for (side in c("lower", "upper")) {
        endpoint <- lv_set_audit_endpoint(
          primary[[key]], entry, failed,
          schema, side, j, tolerance
        )
        kept <- endpoint$kept
        status <- endpoint$status
        schema[[side]][j] <- kept$value
        schema[[paste0(side, "_status")]][j] <- status
        schema[[paste0(side, "_source")]][j] <- if (is.na(kept$source)) {
          kept$origin
        } else {
          paste0(kept$source, "+", kept$origin)
        }
        schema[[paste0(side, "_constraint_residual")]][j] <- kept$residual
        schema[[paste0("arg_", side)]][[j]] <- kept$arg
        rows[[length(rows) + 1L]] <- endpoint$row
      }
    }
    results[[key]] <- primary[[key]]
    results[[key]]$schema <- schema
  }
  list(
    results = results, audit = do.call(rbind, rows),
    selector_provenance = provenance
  )
}

lv_set_audit_endpoint <- function(primary, entry, failed, schema,
                                  side, j, tolerance) {
  first <- lv_set_audit_side(primary$schema, side, j)
  second <- if (failed) NULL else lv_set_audit_side(entry$result$schema, side, j)
  both <- !failed && identical(first$status, "bounded") &&
    identical(second$status, "bounded")
  delta <- if (both) abs(first$value - second$value) else NA_real_
  moved <- both && is.finite(delta) &&
    delta > tolerance * max(1, abs(first$value), abs(second$value))
  reason <- if (failed) {
    if (identical(first$status, "bounded")) "audit_failed" else NA_character_
  } else if (moved) {
    "endpoint_moved"
  } else if (!identical(first$status, second$status)) {
    "status_mismatch"
  } else {
    NA_character_
  }
  status <- if (is.na(reason)) first$status else "unreliable"
  kept <- lv_set_audit_extreme(side, first, second)
  record <- data.frame(
    tau = schema$tau[[j]], coef = schema$coef[[j]],
    side = side, primary_status = first$status,
    audit_status = if (failed) "audit_failed" else second$status,
    final_status = status, primary_value = first$value,
    audit_value = if (failed) NA_real_ else second$value, final_value = kept$value,
    origin = kept$origin, delta = delta, reason = reason,
    detail = if (failed && !is.null(entry$error)) entry$error else NA_character_,
    stringsAsFactors = FALSE
  )
  list(kept = kept, status = status, row = record)
}
