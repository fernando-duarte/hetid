# Ratification-gated projection for the joint moment-compatibility layer
# (joint-GMM, logvar-joint-gmm). Definitions only. Without a ratified moment_delta,
# the projection is disabled and prints its reason -- a tolerance is never
# relabeled a region. With a ratified delta it searches, per target and side,
# for a candidate that satisfies the 2m dimensionless smooth rows
# ±S^{-1} g(b) - delta <= 0; a feasible candidate constructively certifies that
# tolerance region is nonempty, and its absence is reported as no compatible
# candidate found within search, never as a certified empty region. The epigraph
# solve itself is delegated to the driver-wired spec closure so this module owns
# the target/side orchestration and the honest status vocabulary, not the solver.
# Sourced by the joint-GMM test entrypoint and the search/projection driver.

# The ordered projection targets by block: Option A projects the benchmark a_L
# intercept and every slope; Option B additionally projects the PPML intercept
# a_P and the estimated Jensen gap a_P - a_L. b stays diagnostic in both.
logvar_joint_project_targets <- function(block, n_slope) {
  slopes <- if (n_slope > 0L) sprintf("slope_%d", seq_len(n_slope)) else character(0)
  if (identical(block, "log_ppml")) {
    c("a_L", "a_P", slopes, "a_P_minus_a_L")
  } else {
    c("a_L", slopes)
  }
}

# One target/side search at a fixed delta: the driver-wired closure evaluates the
# ordered starts against the 2m smooth rows and returns an attained endpoint with
# its box, floor, and coverage audits. Map that raw result onto the fail-closed
# status vocabulary: bounded only on a stable box-interior attainment, unbounded
# only when a stored direction certificate passes, and unreliable otherwise.
logvar_joint_project_side <- function(spec, target, side, delta) {
  raw <- spec$solve_endpoint(target, side, delta)
  if (is.null(raw) || !isTRUE(raw$found)) {
    return(list(
      target = target, side = side, delta = delta, status = "not_found",
      endpoint = NA_real_, note = "no compatible candidate found within search"
    ))
  }
  status <- if (isTRUE(raw$direction_certified)) {
    "unbounded"
  } else if (isTRUE(raw$box_interior) && isTRUE(raw$floor_stable)) {
    "bounded"
  } else {
    "unreliable"
  }
  list(
    target = target, side = side, delta = delta, status = status,
    endpoint = if (identical(status, "bounded")) raw$value else NA_real_,
    note = if (identical(status, "unreliable")) "box_dependence" else NA_character_
  )
}

# Assemble the min/max hull for one target and delta from its two side searches.
# Equal attained endpoints are described only as an attained projection unchanged
# within search, never as a globally unchanged region.
logvar_joint_project_hull <- function(lo, hi) {
  bounded <- identical(lo$status, "bounded") && identical(hi$status, "bounded")
  if (bounded && isTRUE(abs(hi$endpoint - lo$endpoint) <= 1e-9 * max(1, abs(lo$endpoint)))) {
    return(list(width = 0, status = "attained projection unchanged within search"))
  }
  width <- if (bounded) hi$endpoint - lo$endpoint else NA_real_
  status <- if (bounded) "bounded" else "unreliable"
  list(width = width, status = status)
}

# Gated projection entrypoint: disabled without a ratified delta; otherwise it runs a
# per-target/side/delta search over the smooth tolerance rows. The disabled
# branch returns the exact reason (the joint-gmm driver prints it) so the caller
# records a tolerance, never a region.
logvar_joint_project_set <- function(spec, moment_delta) {
  if (length(moment_delta) == 0L) {
    reason <- "projection disabled: no ratified moment_delta"
    return(invisible(list(status = "disabled", reason = reason, rows = list())))
  }
  targets <- logvar_joint_project_targets(spec$block, spec$n_slope)
  rows <- list()
  for (delta in sort(unique(as.numeric(moment_delta)))) {
    for (target in targets) {
      lo <- logvar_joint_project_side(spec, target, "min", delta)
      hi <- logvar_joint_project_side(spec, target, "max", delta)
      hull <- logvar_joint_project_hull(lo, hi)
      rows[[length(rows) + 1L]] <- list(
        target = target, delta = delta, lo = lo, hi = hi,
        width = hull$width, status = hull$status
      )
    }
  }
  list(status = "enabled", moment_delta = sort(unique(as.numeric(moment_delta))), rows = rows)
}
