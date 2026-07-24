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
