# Non-convex-direction handling for region_envelope() (prepare_region_geometry.R):
# a maturity's quadratic constraint can have leading term a = A_i[perp, perp] <= 0
# along the requested free axis. Not a numerical fluke -- a single A_i has at
# most one positive eigenvalue for any gamma (rank-one PSD minus PSD;
# docs/lewbel_multivariate_set_identification.tex), and a <= 0 gets more
# likely as tau grows. Split out from region_envelope's own file because it
# is a distinct piece of algebra from the convex-constraint intersection that
# function otherwise does.

paper_source_once(paper_path("support", "graphics", "device.R"))

# Raised when a constraint's exclusion splits the free coordinate's feasible
# set into two disjoint pieces. A classed condition, not a bare stopifnot, so
# a caller sweeping over many (tau, axis) combinations can catch this one
# expected failure mode -- via its class, never by matching the message
# string -- and skip just that combination instead of losing the whole sweep.
region_non_convex_error <- function(i, perp, a) {
  structure(
    class = c("region_non_convex_direction", "error", "condition"),
    list(
      message = sprintf(paste(
        "region_envelope: maturity %d's exclusion along axis %d (a = %.6g)",
        "splits the feasible interval into two disjoint pieces; cannot draw",
        "a disconnected region"
      ), i, perp, a),
      call = sys.call(-1)
    )
  )
}

# The two roots of constraint i's free-coordinate quadratic, plus its
# discriminant. Shared by both region_envelope passes, which read the roots
# in opposite ways: convex constraints keep the interval, non-convex exclude it.
region_quadratic_roots <- function(sys, i, perp, k1, k2, X, Y) {
  A <- sys$A[[i]]
  b <- sys$b[[i]]
  a <- A[perp, perp]
  beta <- b[perp] + 2 * (A[perp, k1] * X + A[perp, k2] * Y)
  gam <- sys$c[i] + b[k1] * X + b[k2] * Y +
    A[k1, k1] * X^2 + A[k2, k2] * Y^2 + 2 * A[k1, k2] * X * Y
  disc <- beta^2 - 4 * a * gam
  sq <- sqrt(pmax(disc, 0))
  # divide each root by 2a before taking pmin/pmax: for a < 0 that division
  # flips which of the two is smaller, so ordering the raw numerators first
  # (as the single-root a > 0 case safely could) would silently swap lo/hi
  r1 <- (-beta - sq) / (2 * a)
  r2 <- (-beta + sq) / (2 * a)
  list(a = a, disc = disc, lo = pmin(r1, r2), hi = pmax(r1, r2))
}

# Runs one region_3d draw_fn(ols, units, tau) call, catching
# region_non_convex_direction (a genuine two-piece split, the one case
# region_envelope cannot resolve on its own): a data fact about this
# instrument's region rather than a bug, so the sweep that calls this skips
# it and moves on instead of aborting every other combination. draw_fn may
# have already written a partial figure file before the error, so this
# removes it. Returns the skipped artifact id, or NULL on success.
region_3d_draw_or_skip <- function(draw_fn, ols, units, tau) {
  id <- region_figure_id(ols, units, tau)
  path <- artifact_path(id)
  skipped <- NULL
  tryCatch(
    draw_fn(ols, units, tau),
    region_non_convex_direction = function(e) {
      if (file.exists(path)) file.remove(path)
      message(sprintf(
        "set_id_region_3d: SKIPPED tau=%s units=%s ols=%s -- %s",
        tau, units, ols, conditionMessage(e)
      ))
      skipped <<- id
    }
  )
  # persp cannot be made to fill the device, so a completed figure is trimmed
  # to its own ink here rather than inside draw_fn, where it would run on the
  # partial file above during the unwind.
  if (is.null(skipped)) crop_svg_to_ink(path)
  skipped
}
