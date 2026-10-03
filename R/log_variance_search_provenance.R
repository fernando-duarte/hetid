lv_set_audit_completed <- function(entry) {
  n_raw <- entry$result$diagnostics$n_raw_feasible
  isTRUE(entry$ok) && (is.null(n_raw) || (!is.na(n_raw) && n_raw > 0L))
}

lv_set_selector_provenance <- function(audit, expected = NULL) {
  completed <- vapply(audit, lv_set_audit_completed, logical(1))
  if (!any(completed)) {
    return(list(
      selector_id = NA_character_, traversal = NA_character_,
      status = "all_failed", n_verified = 0L
    ))
  }
  selectors <- lapply(audit[completed], function(entry) entry$result$diagnostics$selector)
  valid <- vapply(selectors, function(value) {
    is.list(value) && is.character(value$selector_id) && length(value$selector_id) == 1L &&
      !is.na(value$selector_id) && identical(value$traversal, "as_selected")
  }, logical(1))
  if (is.null(expected) && !any(valid)) {
    return(list(
      selector_id = NA_character_, traversal = NA_character_,
      status = "not_applicable", n_verified = 0L
    ))
  }
  assert_bad_argument_ok(
    all(valid),
    "coverage selector provenance is absent from a completed engine run", "audit"
  )
  ids <- unique(vapply(selectors, `[[`, character(1), "selector_id"))
  assert_bad_argument_ok(
    length(ids) == 1L && (is.null(expected) || identical(ids, expected)),
    "coverage selector provenance disagrees with its declared protocol", "audit"
  )
  list(
    selector_id = ids, traversal = "as_selected", status = "verified",
    n_verified = sum(completed)
  )
}

lv_set_path_pre_grid_closed <- function(result) {
  mean_closed <- result$diagnostics$closure_reason %in%
    c("mean_domain_unbounded", "mean_domain_unreliable")
  unresolved <- length(result$diagnostics$precheck_failed) > 0L
  zero_rows <- result$diagnostics$zero_rows
  empty_domain <- identical(result$diagnostics$closure_reason, "empty_log_domain") &&
    is.integer(zero_rows) && is.null(dim(zero_rows)) && length(zero_rows) > 0L &&
    !anyNA(zero_rows) && all(zero_rows > 0L)
  isTRUE(mean_closed) || unresolved || empty_domain
}

lv_set_path_raw_count <- function(result) {
  count <- result$diagnostics$n_raw_feasible
  assert_bad_argument_ok(
    is.numeric(count) && is.null(dim(count)) && length(count) == 1L && !is.nan(count),
    "search must retain one raw feasible count", "result"
  )
  if (is.na(count)) {
    assert_bad_argument_ok(
      lv_set_path_pre_grid_closed(result),
      "missing raw feasible count requires a recognized pre-grid closure", "result"
    )
  } else {
    assert_bad_argument_ok(
      is.finite(count) && count >= 0 && count == floor(count) && count <= .Machine$integer.max,
      "raw feasible count must be a nonnegative integer", "result"
    )
  }
  as.integer(count)
}

lv_set_path_closures <- function(results) {
  counts <- vapply(results, lv_set_path_raw_count, integer(1))
  lapply(results[is.na(counts)], function(result) {
    fields <- c("closure_reason", "mean_status", "precheck_failed", "zero_rows")
    result$diagnostics[intersect(fields, names(result$diagnostics))]
  })
}
