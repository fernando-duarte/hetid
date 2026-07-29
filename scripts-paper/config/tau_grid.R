# Shared tau grid for the bounds-by-tau figures. The mean-equation stage and the
# log-variance figure must walk the SAME taus: the mean stage keys its stored
# boxes by paper_tau_key (sprintf "%.17g") and the figure looks them up by that
# key, so grids differing by one ulp hard-fail the figure stage.
#
# Shape: the historical uniform backbone with its final intervals subdivided, so
# the branch switch near tau* renders as a knee rather than one chord. The
# maximum is deliberately unchanged: past the backbone maximum the news box
# explodes while the feasible lattice collapses, so points there would be far
# worse inner approximations drawn in the same ink as their neighbours.

paper_bounds_tau_grid <- function(
  tau_star,
  control = PAPER_FIGURE_RENDER_CONTROL$bounds_tau_grid,
  backbone_n = PAPER_ANALYSIS_CONTRACT$tau$figure_grid_n
) {
  # a missing control field yields logical(0), and stopifnot(logical(0)) PASSES
  # vacuously, so every control is checked for scalar-ness explicitly
  whole_scalar <- function(x, minimum) {
    is.numeric(x) && length(x) == 1L && is.finite(x) &&
      x == as.integer(x) && x >= minimum
  }
  stopifnot(
    is.numeric(tau_star), length(tau_star) == 1L, is.finite(tau_star),
    tau_star > 0,
    whole_scalar(backbone_n, 3L),
    whole_scalar(control$tail_subdivisions, 2L),
    is.numeric(control$tail_fraction), length(control$tail_fraction) == 1L,
    is.finite(control$tail_fraction),
    control$tail_fraction > 0, control$tail_fraction < 1
  )
  backbone <- seq(0, tau_star, length.out = backbone_n)
  backbone <- backbone[backbone > 0 & backbone < tau_star]
  tail_start <- control$tail_fraction * tau_star
  below <- backbone[backbone < tail_start]
  tail_pts <- backbone[backbone >= tail_start]
  # a large tail_fraction or a small backbone_n would leave one side empty and
  # silently degenerate into a reversed sequence with duplicate keys
  stopifnot(length(below) >= 1L, length(tail_pts) >= 1L)
  ends <- c(max(below), tail_pts)
  dense <- unlist(lapply(seq_len(length(ends) - 1L), function(i) {
    seq(ends[i], ends[i + 1L], length.out = control$tail_subdivisions + 1L)[-1L]
  }))
  grid <- sort(c(below, dense))
  stopifnot(
    all(diff(grid) > 0), all(grid > 0), all(grid < tau_star),
    # exact, not all.equal: paper_tau_key is %.17g and the cap must round-trip
    identical(max(grid), max(backbone)),
    !anyDuplicated(vapply(grid, paper_tau_key, character(1))),
    # exact cardinality, so a silently smaller grid cannot pass the other checks
    length(grid) ==
      length(below) + (length(ends) - 1L) * control$tail_subdivisions
  )
  grid
}
