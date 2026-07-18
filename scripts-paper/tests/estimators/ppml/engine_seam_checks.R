# Engine cold-start, start-pool, and grid-selector seam checks.

check("peng cold-start replication flags a warm-dependent dummy", peng_try({
  schema <- peng_run(peng_dummy(cold_disagree = TRUE)$est, cold = TRUE)$schema
  any(c(schema$lower_status, schema$upper_status) == "unreliable")
}))

check("peng starts_per_side default uses the primary control", peng_try(identical(
  peng_run(peng_dummy()$est)$table,
  peng_run(
    peng_dummy()$est,
    starts_per_side = LOGVAR_SEARCH_CONTROL$primary_starts_per_side
  )$table
)))
check("peng generic scan accepts starts_per_side = 3L", peng_try({
  result <- peng_run(peng_dummy()$est, starts_per_side = 3L)
  is.data.frame(result$table) && nrow(result$table) == 2L
}))

# A valid subset in engine-default traversal is accepted and identified;
# duplicate and invented rows are rejected.
peng_subset <- peng_sel("sel-subset", "engine_default", function(g) {
  g[seq_len(max(1L, ceiling(nrow(g) / 2L))), , drop = FALSE]
})
peng_dup <- peng_sel("sel-dup", "engine_default", function(g) {
  g[c(1L, seq_len(nrow(g))), , drop = FALSE]
})
peng_invent <- peng_sel(
  "sel-invent",
  "engine_default",
  function(g) rbind(g, c(5, 5))
)
check("peng grid_selector = NULL preserves the default table", peng_try(identical(
  peng_run(peng_dummy()$est)$table,
  peng_run(peng_dummy()$est, grid_selector = NULL)$table
)))
check("peng grid_selector accepts a valid subset and records its id", peng_try({
  result <- peng_run(peng_dummy()$est, grid_selector = peng_subset)
  is.data.frame(result$table) && nrow(result$table) == 2L &&
    identical(result$diagnostics$selector$selector_id, "sel-subset") &&
    identical(result$diagnostics$selector$traversal, "engine_default")
}))
check("peng grid_selector rejects duplicate or invented rows", peng_try({
  bad <- function(selector, expected) {
    message <- tryCatch(
      peng_run(peng_dummy()$est, grid_selector = selector),
      error = conditionMessage
    )
    is.character(message) && grepl(expected, message, fixed = TRUE)
  }
  bad(peng_dup, "duplicate rows") && bad(peng_invent, "invented rows")
}))
