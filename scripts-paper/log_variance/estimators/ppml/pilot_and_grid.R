# Artifact-free driver helpers for the PPML log-variance map: the deterministic
# Morton-order space-filling grid selector the independent coverage gate feeds
# to the engine seam, and the pre-mapping scaling pilot that decides whether the
# fixed response scale switches on before any production fit. The coverage gate
# itself (the fresh-cache larger-grid run and the atomic union/demotion apply)
# lives in coverage.R, sourced below to keep both files under the
# hard 200-line cap. Definitions only; sourced after estimator.R (for
# logvar_ppml_fit) and the engine, before the driver run_sets.R.

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

paper_source_once(paper_path("log_variance", "estimators", "ppml", "coverage.R"))

# Deterministic space-filling selector usable directly as the engine's
# grid_selector: normalize each coordinate to its own range over the feasible
# grid (a zero-span coordinate maps to all zeros), quantize to 17 bits, and
# interleave the columns' bit strings arithmetically into an exactly represented
# Morton key (bit b of coordinate k lands at position b*K + k; the <= 51-bit
# total stays exact in a double, so the key is built with %/% and %% rather than
# bitwAnd, whose 32-bit range the key exceeds). Order by that key with the
# full-precision lexicographic b key as tiebreaker, then take equally spaced
# sorted ranks up to the cap. The returned grid is always an exact row subset of
# the input (rows are selected by index, never transformed), so the engine's
# subset check accepts it; traversal "as_selected" scans in this order and
# bypasses the nearest-neighbor cap.
logvar_ppml_morton_select <- function(
  b_feas,
  max_pts,
  control = LOGVAR_PPML_CONTROL
) {
  n <- nrow(b_feas)
  k_cols <- ncol(b_feas)
  bits <- control$morton_bits
  if (bits * k_cols > control$exact_double_bits) {
    stop("logvar_ppml_morton_select: Morton key exceeds exact double range")
  }
  qmax <- 2^bits - 1
  q <- matrix(0, n, k_cols)
  for (k in seq_len(k_cols)) {
    rng <- range(b_feas[, k])
    span <- rng[2L] - rng[1L]
    if (span > 0) q[, k] <- floor((b_feas[, k] - rng[1L]) / span * qmax)
  }
  key <- numeric(n)
  for (k in seq_len(k_cols)) {
    qk <- q[, k]
    for (bpos in 0:(bits - 1L)) {
      key <- key + ((qk %/% (2^bpos)) %% 2) * 2^(bpos * k_cols + (k - 1L))
    }
  }
  bkey <- do.call(paste, c(
    lapply(seq_len(k_cols), function(k) {
      paper_numeric_key(b_feas[, k])
    }),
    sep = "|"
  ))
  ord <- order(key, bkey, method = "radix")
  m <- if (is.null(max_pts)) n else min(max_pts, n)
  ranks <- 1L + floor((0:(m - 1L)) * (n / m))
  list(
    grid = b_feas[ord[ranks], , drop = FALSE],
    selector_id = LOGVAR_PPML_COVERAGE_PROTOCOL$selector_id,
    traversal = LOGVAR_PPML_COVERAGE_PROTOCOL$traversal
  )
}

# Pre-mapping scaling pilot: fit the scale anchor and up to the first ten rows of
# the coarsened baseline-tau feasible grid at response_scale = 1, and flip the
# fixed response scale on if any fit trips a ratified instability trigger --
# a non-converged accept verdict, a non-finite or above-1e10 column-normalized
# information condition, or a warm-start linear predictor within 5 of double
# overflow. When scaling is needed the scale is the median positive anchor
# response; if the anchor has no positive response the pilot stops fail-closed
# rather than inventing one. The pilot is deterministic and its verdict prints
# either way; on this sample it is expected to leave scaling dormant (s = 1).
logvar_ppml_pilot <- function(
  w1,
  w2,
  x_mat,
  anchor_b,
  grid_pts,
  control = LOGVAR_PPML_CONTROL
) {
  overflow_guard <-
    log(.Machine$double.xmax) - control$pilot_overflow_margin
  eval_one <- function(b, source) {
    fit <- logvar_ppml_fit(
      b,
      w1,
      w2,
      x_mat,
      response_scale = 1,
      control = control
    )
    cws <- fit$diagnostics$condition_weighted_scaled
    trig_cond <- length(cws) != 1L ||
      !is.finite(cws) ||
      cws > control$pilot_condition_limit
    trig_eta <- !is.null(fit$warm_start) &&
      max(drop(x_mat %*% fit$warm_start)) > overflow_guard
    list(
      source = source, converged = isTRUE(fit$converged),
      trigger = isTRUE(!isTRUE(fit$converged) || trig_cond || trig_eta)
    )
  }
  records <- list(eval_one(anchor_b, "anchor"))
  n_grid <- if (is.null(grid_pts)) {
    0L
  } else {
    min(control$pilot_grid_points, nrow(grid_pts))
  }
  for (i in seq_len(n_grid)) {
    records[[length(records) + 1L]] <- eval_one(grid_pts[i, ], "grid")
  }
  n_triggered <- sum(vapply(records, function(r) r$trigger, logical(1)))
  scale_needed <- n_triggered > 0L
  response_scale <- 1
  if (scale_needed) {
    y_anchor <- drop(w1 - w2 %*% anchor_b)^2
    pos <- y_anchor[y_anchor > 0]
    if (length(pos) == 0L) {
      stop("logvar_ppml_pilot: scale needed but anchor response has no positive value")
    }
    response_scale <- stats::median(pos)
  }
  list(
    scale_needed = scale_needed, response_scale = response_scale,
    n_fits = length(records), n_triggered = n_triggered, records = records
  )
}
