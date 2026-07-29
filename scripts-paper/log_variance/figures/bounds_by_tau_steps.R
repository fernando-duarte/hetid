# Shape diagnostic for the bounds-by-tau grid: does the lower boundary descend
# through a branch switch as a knee, or as one cliff? A cliff is a single
# adjacent step that dwarfs the steps either side of it, which is what a switch
# looks like when no grid point falls inside it; refining the grid spreads the
# same descent over several comparable steps. Reporting the largest such ratio
# makes the claim about shape a number rather than a reading of the panel.
# Definitions only; sourced by render_bounds_by_tau.R.

# Two degeneracies have to be refused rather than reported, because both produce
# a large ratio with no cliff behind it. A downgraded row leaves a hole, and a
# difference taken across it spans two grid steps rather than one, so steps are
# measured only inside runs of neighbouring taus. And a repeated endpoint makes
# a neighbouring step exactly zero, which would divide a finite step by nothing;
# those positions carry no verdict and are dropped instead of floored.
logvar_bounds_tau_steps <- function(rows) {
  grid_tau <- sort(unique(rows$tau))
  bounded <- rows[rows$lower_status == PAPER_ENDPOINT_STATUS[["bounded"]], ]
  if (nrow(bounded) == 0L) {
    return(numeric(0))
  }
  vapply(split(bounded, bounded$coef), function(s) {
    ord <- order(s$tau)
    idx <- match(s$tau[ord], grid_tau)
    val <- s$lower[ord]
    runs <- split(seq_along(idx), cumsum(c(1L, diff(idx) != 1L)))
    ratios <- unlist(lapply(runs, function(k) {
      if (length(k) < 4L) {
        return(numeric(0))
      }
      d <- abs(diff(val[k]))
      neighbour <- pmax(c(d[-1L], 0), c(0, d[-length(d)]))
      keep <- neighbour > 0
      d[keep] / neighbour[keep]
    }), use.names = FALSE)
    if (!length(ratios)) NA_real_ else max(ratios)
  }, numeric(1))
}

# "none" rather than an empty line: a map whose lower side is unbounded across
# the whole grid has no boundary to measure, which is a different statement from
# a measurement that came back empty
logvar_bounds_tau_steps_report <- function(steps) {
  if (!length(steps)) {
    return("none (no bounded lower side on the grid)")
  }
  paste(sprintf("%s %.1f", names(steps), steps), collapse = "; ")
}
