# Artifact-free driver helpers for the PPML log-variance map: the deterministic
# Morton-order space-filling grid selector the independent coverage gate feeds
# to the engine seam, and the pre-mapping scaling pilot that decides whether the
# fixed response scale switches on before any production fit. The coverage gate
# itself (the fresh-cache larger-grid run and the atomic union/demotion apply)
# lives in coverage.R, sourced below to keep both files under the
# hard 200-line cap. Definitions only; sourced after estimator.R (for
# logvar_ppml_fit) and the engine, before the driver run_sets.R.

source(paper_path("log_variance", "estimators", "ppml", "coverage.R"))

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
logvar_ppml_morton_select <- function(b_feas, max_pts) {
  n <- nrow(b_feas)
  k_cols <- ncol(b_feas)
  bits <- 17L
  if (bits * k_cols > 53L) {
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
  bkey <- do.call(paste, c(lapply(seq_len(k_cols), function(k) {
    formatC(b_feas[, k], digits = 17L, format = "fg", flag = "#")
  }), sep = "|"))
  ord <- order(key, bkey, method = "radix")
  m <- if (is.null(max_pts)) n else min(max_pts, n)
  ranks <- 1L + floor((0:(m - 1L)) * (n / m))
  list(
    grid = b_feas[ord[ranks], , drop = FALSE],
    selector_id = "morton-v1", traversal = "as_selected"
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
logvar_ppml_pilot <- function(w1, w2, x_mat, anchor_b, grid_pts) {
  overflow_guard <- log(.Machine$double.xmax) - 5
  eval_one <- function(b, source) {
    fit <- logvar_ppml_fit(b, w1, w2, x_mat, response_scale = 1)
    cws <- fit$diagnostics$condition_weighted_scaled
    trig_cond <- length(cws) != 1L || !is.finite(cws) || cws > 1e10
    trig_eta <- !is.null(fit$warm_start) &&
      max(drop(x_mat %*% fit$warm_start)) > overflow_guard
    list(
      source = source, converged = isTRUE(fit$converged),
      trigger = isTRUE(!isTRUE(fit$converged) || trig_cond || trig_eta)
    )
  }
  records <- list(eval_one(anchor_b, "anchor"))
  n_grid <- if (is.null(grid_pts)) 0L else min(10L, nrow(grid_pts))
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
