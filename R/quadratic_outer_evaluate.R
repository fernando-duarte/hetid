# Evaluation helpers for the outer_bounds closure: objective scaling, the
# tightest verified bound per side, per-side refinement and the result.

# Power-of-two scaled columns for every nonzero objective that scales exactly.
outer_scale_objectives <- function(objectives, zero) {
  exps <- vapply(seq_len(ncol(objectives)), function(j) {
    outer_pow2_exponent(objectives[, j])
  }, numeric(1))
  scaled <- lapply(seq_len(ncol(objectives)), function(j) {
    outer_pow2_scale(objectives[, j], exps[j])
  })
  active <- which(!zero & !vapply(scaled, is.null, logical(1)))
  list(active = active, exps = exps[active], matrix = do.call(cbind, scaled[active]))
}

outer_best_bounds <- function(cands, scaled) {
  width <- ncol(scaled)
  best <- list(
    lower = rep(-Inf, width), upper = rep(Inf, width),
    src_lower = rep(NA_integer_, width), src_upper = rep(NA_integer_, width),
    cands = cands
  )
  for (i in seq_along(cands)) {
    best <- outer_merge_bounds(
      best, outer_candidate_bounds(cands[[i]], scaled), i,
      seq_len(width)
    )
  }
  best
}

outer_merge_bounds <- function(best, found, index, cols) {
  hi <- !is.na(found$upper) & found$upper < best$upper[cols]
  lo <- !is.na(found$lower) & found$lower > best$lower[cols]
  best$upper[cols[hi]] <- found$upper[hi]
  best$lower[cols[lo]] <- found$lower[lo]
  best$src_upper[cols[hi]] <- index
  best$src_lower[cols[lo]] <- index
  best
}

# Per-side searches from each side's current best candidate; a result is
# verified before it can tighten the side.
outer_refine_sides <- function(sys, best, scaled, maxit) {
  for (col_index in seq_len(ncol(scaled))) {
    for (side in c(1, -1)) {
      from <- if (side > 0) best$src_upper[col_index] else best$src_lower[col_index]
      from_weights <- best$cands[[if (is.na(from)) 1L else from]]$v
      side_weights <- outer_side_search(sys, side * scaled[, col_index], from_weights, maxit)
      found <- outer_verify(sys, side_weights)
      if (is.null(found) || found$empty) next
      best$cands[[length(best$cands) + 1L]] <- found
      side_bounds <- outer_candidate_bounds(found, scaled[, col_index, drop = FALSE])
      best <- outer_merge_bounds(best, side_bounds, length(best$cands), col_index)
    }
  }
  best
}

# Undo the objective scaling exactly and pad by the smallest normal number,
# which covers rounding of any result that lands in the subnormal range.
outer_rescale_bounds <- function(bounds, best, scaled) {
  tiny <- .Machine$double.xmin
  active <- scaled$active
  bounds$lower[active] <- best$lower * 2^scaled$exps - tiny
  bounds$upper[active] <- best$upper * 2^scaled$exps + tiny
  bounds$src_lower[active] <- best$src_lower
  bounds$src_upper[active] <- best$src_upper
  bounds
}

outer_bounds_result <- function(bounds, cands, empty, reason) {
  out <- data.frame(lower = bounds$lower, upper = bounds$upper)
  attr(out, "pool") <- lapply(cands[-1L], `[[`, "v")
  attr(out, "sources") <- data.frame(lower = bounds$src_lower, upper = bounds$src_upper)
  attr(out, "candidates") <- data.frame(
    lambda = vapply(cands, `[[`, 0, "lam"), eta = vapply(cands, `[[`, 0, "eta"),
    e_q = vapply(cands, `[[`, 0, "e_q"), r_bar = vapply(cands, `[[`, 0, "r_bar"),
    rho_bar = vapply(cands, `[[`, 0, "rho_bar")
  )
  attr(out, "empty") <- empty
  attr(out, "reason") <- reason
  out
}
