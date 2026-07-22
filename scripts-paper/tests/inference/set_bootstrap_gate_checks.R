# Structured-status failure gate: a cell whose collected endpoint statuses
# cross the canonical fatal-failure share must stop() by run label; one just
# under the threshold must pass and report its true share.

gate_limit <- PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$fatal_failure_share
gate_cell <- function(n_failed, n_b) {
  status <- c(
    rep(PAPER_ENDPOINT_STATUS[["failed"]], n_failed),
    rep(PAPER_ENDPOINT_STATUS[["bounded"]], n_b - n_failed)
  )
  list(
    lower = matrix(0, n_b, 1), upper = matrix(0, n_b, 1),
    lower_status = matrix(status, n_b, 1), upper_status = matrix(status, n_b, 1)
  )
}
gate_n_b <- 200L
gate_under_n <- floor((gate_limit - 0.01) * gate_n_b)
gate_over_n <- ceiling((gate_limit + 0.01) * gate_n_b)

gate_under <- list(est = list(gate_cell(gate_under_n, gate_n_b)))
gate_under_cells <- logvar_boot_failure_gate(gate_under, "est", "fixture-under")
gate_under_share <- unname(gate_under_cells[["est"]][1L, "share"])
check(
  "structured failure gate passes and reports the true share just under the threshold",
  isTRUE(all.equal(gate_under_share, gate_under_n / gate_n_b)) &&
    gate_under_share < gate_limit
)

gate_over <- list(est = list(gate_cell(gate_over_n, gate_n_b)))
gate_error <- tryCatch(
  {
    logvar_boot_failure_gate(gate_over, "est", "fixture-over")
    "no error"
  },
  error = function(error) conditionMessage(error)
)
check(
  "structured failure gate stops by run label on a cell over the threshold",
  !identical(gate_error, "no error") && grepl("fixture-over", gate_error, fixed = TRUE)
)

rm(
  gate_limit, gate_cell, gate_n_b, gate_under_n, gate_over_n,
  gate_under, gate_under_cells, gate_under_share, gate_over, gate_error
)

# Cache validator: a payload with primary and sensitivity collected draws
# matching in estimators/tau axis/row counts and a closed status vocabulary
# must validate; disturbing any one of those must return a reason string.
cache_b <- 8L
cache_ests <- c("ppml", "harvey")
cache_cell <- function(n_b) {
  status <- rep(PAPER_ENDPOINT_STATUS[["bounded"]], n_b)
  list(
    lower = matrix(0, n_b, 1), upper = matrix(0, n_b, 1),
    lower_status = matrix(status, n_b, 1), upper_status = matrix(status, n_b, 1)
  )
}
cache_coll <- function(n_b, n_tau = 3L) {
  per_est <- lapply(seq_len(n_tau), function(...) cache_cell(n_b))
  stats::setNames(rep(list(per_est), length(cache_ests)), cache_ests)
}
cache_ok <- list(
  collected = cache_coll(cache_b),
  sens_collected = cache_coll(cache_b),
  provenance = list(b_reps = cache_b, sens_reps = cache_b)
)
check(
  "well-formed cache payload validates",
  isTRUE(logvar_boot_cache_validate(cache_ok))
)

cache_missing <- cache_ok
cache_missing$sens_collected <- NULL
cache_missing_reason <- logvar_boot_cache_validate(cache_missing)
check(
  "cache validator names a missing sens_collected field",
  is.character(cache_missing_reason) &&
    grepl("sens_collected", cache_missing_reason, fixed = TRUE)
)

cache_bad_rows <- cache_ok
cache_bad_rows$sens_collected <- cache_coll(cache_b - 1L)
check(
  "cache validator rejects a sensitivity row count off sens_reps",
  is.character(logvar_boot_cache_validate(cache_bad_rows))
)

cache_bad_tau <- cache_ok
cache_bad_tau$sens_collected$ppml <- cache_bad_tau$sens_collected$ppml[-1L]
check(
  "cache validator rejects mismatched primary/sensitivity tau axes",
  is.character(logvar_boot_cache_validate(cache_bad_tau))
)

cache_bad_status <- cache_ok
cache_bad_status$sens_collected$ppml[[1L]]$lower_status[1, 1] <- "garbage"
check(
  "cache validator rejects an out-of-vocabulary endpoint status",
  is.character(logvar_boot_cache_validate(cache_bad_status))
)

rm(
  cache_b, cache_ests, cache_cell, cache_coll, cache_ok,
  cache_missing, cache_missing_reason, cache_bad_rows, cache_bad_tau, cache_bad_status
)
