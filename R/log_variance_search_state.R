lv_set_mutable_environment <- function(value, fields, arg) {
  assert_bad_argument_ok(
    is.environment(value) && !identical(value, emptyenv()) &&
      !environmentIsLocked(value),
    paste(arg, "must be an unlocked mutable environment"), arg
  )
  for (field in intersect(fields, ls(value, all.names = TRUE))) {
    assert_bad_argument_ok(
      !bindingIsLocked(field, value) && !bindingIsActive(field, value),
      paste(arg, "must have ordinary mutable state bindings"), arg
    )
  }
  invisible(TRUE)
}

lv_set_validate_cache <- function(cache) {
  fields <- c("estimator", "sample_id", "spec_id", "store")
  lv_set_mutable_environment(cache, fields, "cache")
  present <- fields %in% ls(cache, all.names = TRUE)
  assert_bad_argument_ok(!any(present) || all(present), "cache binding is incomplete", "cache")
  if (!any(present)) {
    return(invisible(TRUE))
  }
  for (field in fields[1:3]) {
    value <- cache[[field]]
    assert_bad_argument_ok(is.character(value) && length(value) == 1L &&
      !is.na(value) && nzchar(value), "cache identity is invalid", "cache")
  }
  lv_set_mutable_environment(cache$store, character(), "cache$store")
  invisible(TRUE)
}

lv_set_validate_budget <- function(budget) {
  template <- lv_set_budget()
  fields <- ls(template, all.names = TRUE)
  lv_set_mutable_environment(budget, fields, "budget")
  assert_bad_argument_ok(
    all(fields %in% ls(budget, all.names = TRUE)),
    "budget must be a returned search budget", "budget"
  )
  lv_set_validate_budget_limit(budget$max_fit_evals)
  count_fields <- c("n_attempted", "n_evaluated", "n_cached", "n_failed")
  for (field in count_fields) lv_set_validate_count(budget[[field]], field)
  counters <- budget$counters
  assert_bad_argument_ok(is.numeric(counters) && is.null(dim(counters)) &&
    identical(names(counters), names(template$counters)), "invalid budget counters", "budget")
  for (value in counters) lv_set_validate_count(value, "budget counters")
  assert_bad_argument_ok(
    budget$n_evaluated <= budget$max_fit_evals &&
      budget$n_failed <= budget$n_evaluated &&
      sum(counters[1:4]) == budget$n_evaluated && counters[["cache_hit"]] == budget$n_cached &&
      budget$n_attempted == budget$n_evaluated + budget$n_cached,
    "budget counts are inconsistent", "budget"
  )
  invisible(TRUE)
}

lv_set_validate_count <- function(value, arg) {
  lv_set_validate_budget_limit(value)
  assert_bad_argument_ok(is.finite(value), paste(arg, "must be finite"), "budget")
}

lv_set_validate_overrides <- function(cold_start_check, tau, cache, budget) {
  assert_flag(cold_start_check, "cold_start_check")
  assert_bad_argument_ok(is.numeric(tau) && length(tau) == 1L && is.null(dim(tau)) &&
    (is.na(tau) || is.finite(tau)), "tau must be a finite scalar label or NA", "tau")
  assert_bad_argument_ok(!is.nan(tau), "tau must not be NaN", "tau")
  lv_set_validate_cache(cache)
  lv_set_validate_budget(budget)
  invisible(TRUE)
}
