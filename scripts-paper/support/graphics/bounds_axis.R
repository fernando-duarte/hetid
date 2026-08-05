# Shared display cap for the bounds-by-tau x axis, used by both the mean-equation
# figure and the log-variance ones so the two exhibits are cut the same way.
#
# Approaching tau* the branch switch turns the sampled boundary near-vertical,
# and the panel then reads as a spike with everything below it flattened onto the
# axis. paper_bounds_tau_grid subdivides its final backbone intervals precisely
# because that switch lives there, so the subdivided tail and the kink are the
# same points: walking back over the trailing run of short gaps and stopping at
# the last full-width one lands on the boundary by construction, tracks tau*
# instead of going stale against a hard-coded slack, and needs no configuration.
#
# Callers apply the result through coord_cartesian, which zooms rather than
# filters: no tau, no grid, no bound and no tau* changes, and every row stays in
# the data, in the assertions and in the written artifacts.
#
# A grid with no subdivided tail has no short gaps, so the cap is the largest
# sampled tau and the axis is not truncated at all. Definitions only.

paper_bounds_tau_display_cap <- function(sampled_taus) {
  sampled_taus <- sort(unique(sampled_taus))
  stopifnot(
    is.numeric(sampled_taus), length(sampled_taus) >= 3L,
    all(is.finite(sampled_taus))
  )
  gaps <- diff(sampled_taus)
  # half the median gap separates a subdivided interval from a backbone one for
  # any subdivision count of two or more, which paper_bounds_tau_grid enforces
  fine <- gaps < 0.5 * stats::median(gaps)
  if (!any(fine)) {
    return(max(sampled_taus))
  }
  coarse <- which(!fine)
  stopifnot(length(coarse) >= 1L)
  sampled_taus[max(coarse) + 1L]
}
