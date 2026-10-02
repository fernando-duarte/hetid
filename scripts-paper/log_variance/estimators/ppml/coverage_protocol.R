# Definitions the coverage apply (coverage.R) needs and its callers share: the
# PPML coverage protocol, the selector-provenance derivation, and the
# provenance-suffix helper. Definitions only; sourced by coverage.R, so any
# caller of the apply (PPML, Harvey, the log projections) gets them with it.

LOGVAR_PPML_COVERAGE_PROTOCOL <- list(
  schema_version = "1.0.0",
  selector_id = "morton-v1",
  traversal = "as_selected"
)

# Derive selector provenance only from completed engine diagnostics. PPML
# supplies its expected protocol and therefore fails on absent or mismatched
# diagnostics. Generic users of the atomic coverage apply may omit an expected
# protocol; runs with no selector diagnostics are then explicitly not
# applicable. With no completed run, no selector provenance is invented.
.logvar_ppml_selector_provenance <- function(coverage, expected = NULL) {
  completed <- vapply(coverage, function(x) isTRUE(x$ok), logical(1))
  if (!any(completed)) {
    return(list(
      selector_id = NA_character_, traversal = NA_character_,
      status = "all_failed", n_verified = 0L
    ))
  }
  selectors <- lapply(coverage[completed], function(x) x$res$diagnostics$selector)
  valid <- vapply(selectors, function(x) {
    is.list(x) &&
      length(x$selector_id) == 1L && is.character(x$selector_id) &&
      !is.na(x$selector_id) &&
      length(x$traversal) == 1L && is.character(x$traversal) &&
      !is.na(x$traversal)
  }, logical(1))
  if (is.null(expected) && !any(valid)) {
    return(list(
      selector_id = NA_character_, traversal = NA_character_,
      status = "not_applicable", n_verified = 0L
    ))
  }
  if (!all(valid)) {
    stop("coverage selector provenance is absent from a completed engine run")
  }
  ids <- unique(vapply(selectors, `[[`, character(1), "selector_id"))
  traversals <- unique(vapply(selectors, `[[`, character(1), "traversal"))
  if (length(ids) != 1L || length(traversals) != 1L) {
    stop("coverage selector provenance disagrees across completed engine runs")
  }
  if (!is.null(expected) &&
    (!identical(ids[[1L]], expected$selector_id) ||
      !identical(traversals[[1L]], expected$traversal))) {
    stop("coverage selector provenance does not match its declared protocol")
  }
  list(
    selector_id = ids[[1L]], traversal = traversals[[1L]],
    status = "verified", n_verified = sum(completed)
  )
}

# suffix a provenance string with the source it came from; a missing provenance
# collapses to the bare source label
.logvar_prov_suffix <- function(prov, source) {
  if (is.null(prov) || length(prov) != 1L || is.na(prov)) {
    return(source)
  }
  paste0(prov, "+", source)
}
