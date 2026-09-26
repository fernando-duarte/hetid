# Residual-zero census over the joint set, split from residual_map.R. Row t's
# hyperplane w2_t' b = w1_t meets the set iff w1_t lies in the image of the
# functional w2_t' b over it. A no-crossing verdict therefore needs bounds that
# contain that image: the caller's containing box screens first, then the
# verified outer bounds of each remaining row. A crossing is flagged when w1_t
# lies within the range attained at checked member points, which is exact on a
# connected set and otherwise errs toward a crossing. Every other row is
# unresolved, and callers fail closed on it. lower/upper must be containing
# bounds (paper_containing_box), never attained endpoints.

paper_source_once(paper_path(
  "support", "identification", "profile_evidence.R"
))

logvar_crossing_census <- function(qs, lower, upper, w1, w2) {
  # relative slack biased toward flagging a crossing, so roundoff at the
  # functional endpoints never certifies a false negative
  slack <- PAPER_QUADRATIC_CONTROL$crossing_range_rtol * pmax(1, abs(w1))
  rows <- which(!logvar_census_box_clear(lower, upper, w1, w2, slack))
  # a zero w2 row reaches here only when w1_t is within the slack of 0 (its
  # box range is {0}), i.e. eps_t is zero over the whole set up to roundoff: a
  # crossing, and one a zero-objective functional bound could not certify
  zero <- rows[rowSums(w2[rows, , drop = FALSE] != 0) == 0L]
  rows <- setdiff(rows, zero)
  if (!length(rows)) {
    return(list(cross = zero, unresolved = integer(0)))
  }
  slack <- slack[rows]
  loadings <- t(w2[rows, , drop = FALSE])
  evidence <- paper_profile_evidence(qs, loadings)
  # one verified candidate pool per system, grown on the coordinate sides and
  # reused for every row; only rows it cannot clear get their own search
  pool <- if (is.null(evidence$boundedness)) {
    NULL
  } else {
    attr(evidence$outer_bounds(diag(nrow(loadings)), refine = TRUE), "pool")
  }
  clear <- logvar_census_clear(evidence, loadings, w1[rows], slack, pool, FALSE)
  cross <- zero
  pending <- integer(0)
  for (j in which(!clear)) {
    if (logvar_census_attained(qs, evidence, loadings[, j], j, w1[rows[j]], slack[j])) {
      cross <- c(cross, rows[j])
    } else {
      pending <- c(pending, j)
    }
  }
  if (length(pending)) {
    cleared <- logvar_census_clear(
      evidence, loadings[, pending, drop = FALSE],
      w1[rows[pending]], slack[pending], pool, TRUE
    )
    pending <- pending[!cleared]
  }
  list(cross = sort(cross), unresolved = rows[pending])
}

# Rows the containing box proves cannot cross. Each box range is a floating
# dot product that can cancel when the box sits far from the origin, so a row
# clears only beyond it by the slack plus that product's rounding bound, which
# scales with sum(|w2| * max(|lower|, |upper|)). Overflow or NA clears nothing.
logvar_census_box_clear <- function(lower, upper, w1, w2, slack) {
  w2_pos <- pmax(w2, 0)
  w2_neg <- pmin(w2, 0)
  box_min <- drop(w2_pos %*% lower + w2_neg %*% upper)
  box_max <- drop(w2_pos %*% upper + w2_neg %*% lower)
  k <- 2 * ncol(w2) + 4
  g <- k * .Machine$double.eps / (2 - k * .Machine$double.eps)
  rounding <- g * drop(abs(w2) %*% pmax(abs(lower), abs(upper))) * (1 + g)
  clear <- w1 < box_min - slack - rounding | w1 > box_max + slack + rounding
  clear & !is.na(clear)
}

# Rows whose verified outer range excludes w1 beyond the slack. Without a
# verified certificate every bound is NA and no row is cleared.
logvar_census_clear <- function(evidence, loadings, w1, slack, pool, refine) {
  if (is.null(evidence$boundedness)) {
    return(rep(FALSE, length(w1)))
  }
  outer <- evidence$outer_bounds(loadings, refine = refine, pool = pool)
  (!is.na(outer$lower) & w1 < outer$lower - slack) |
    (!is.na(outer$upper) & w1 > outer$upper + slack)
}

# TRUE when w1 lies within the attained range of the functional, both sides
# found at checked member points; a failed or unbounded side is not a range.
logvar_census_attained <- function(qs, evidence, loading, index, w1, slack) {
  fmin <- solve_linear_functional_bound(qs, loading, "min",
    evidence = evidence, evidence_index = index
  )
  if (!(fmin$bounded && fmin$valid) || w1 < fmin$bound - slack) {
    return(FALSE)
  }
  fmax <- solve_linear_functional_bound(qs, loading, "max",
    evidence = evidence, evidence_index = index
  )
  fmax$bounded && fmax$valid && w1 <= fmax$bound + slack
}
