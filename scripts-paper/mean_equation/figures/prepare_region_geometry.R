# Shared helpers for the identified-region figures (render_projections.R and
# render_region_3d.R). Rescales the mean-equation quadratic system to
# standard-deviation units and gives the closed-form projection envelope of the
# joint identified set.
#
# Each axis is the standardized coefficient b_{i,N} * sd(PC_{N,i}); the set
# {b : b'A_i b + b_i.b + c_i <= 0, i} maps to beta = S b via
# A -> S^{-1} A S^{-1}, b -> S^{-1} b, c unchanged (S = diag of the news-PC sds).
#
# The set is a convex intersection of quadric interiors, and each constraint is
# a univariate quadratic in the free (projected-out) coordinate t. So for a
# plotted coordinate pair the free-coordinate feasible set of constraint i is an
# interval [lo_i, hi_i]; the plate exists where L = max_i lo_i <= H = min_i hi_i,
# its projected boundary is the zero level of M = L - H, and its two skins are
# z = L (bottom) and z = H (top). This is exact and vectorized -- no optimizer.
# Run via run_pipeline.R after estimate_identified_set.R.

source(paper_path("support", "identification", "api.R"))
source(paper_path("support", "identification", "tau_star.R"))

# news-PC standard deviations that define the SD-unit axes (Y2 = w2 = PC_N under
# the beta2R = 0 null, so sd(w2[, i]) = sd(PC_{N,i}))
region_sd <- apply(set_id_mean_eq$w2, 2, stats::sd)

# SD-unit quadratic system {A_i, b_i, c_i} at a given slack tau
region_sd_system <- function(tau) {
  qs <- tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
  s <- region_sd
  list(
    A = lapply(qs$A_i, function(m) m / outer(s, s)),
    b = lapply(qs$b_i, function(v) v / s),
    c = as.numeric(unlist(qs$c_i))
  )
}

# per-coefficient bounding box of the set at slack tau, in SD units (the
# reported profile-bound intervals from set_id_mean_eq scaled by the sds)
region_sd_box <- function(tau) {
  theta <- set_id_mean_eq$set_tables[[sprintf("tau_%.2g", tau)]]$theta
  list(lo = theta$set_lower * region_sd, hi = theta$set_upper * region_sd)
}

# tau = 0 point in SD units
region_sd_point <- function() set_id_mean_eq$theta_table$point * region_sd

# Closed-form free-coordinate envelope over an (k1, k2) grid (matrices X, Y for
# the two kept axes, k1 < k2); perp is the projected-out axis. Returns the
# bottom skin L, top skin H, and margin M = L - H (<= 0 inside the projection).
region_envelope <- function(sys, perp, X, Y) {
  keep <- setdiff(1:3, perp)
  k1 <- keep[1]
  k2 <- keep[2]
  big <- 1e6 # out-of-domain sentinel
  L <- matrix(-Inf, nrow(X), ncol(X))
  H <- matrix(Inf, nrow(X), ncol(X))
  for (i in seq_along(sys$A)) {
    A <- sys$A[[i]]
    b <- sys$b[[i]]
    a <- A[perp, perp]
    # each constraint is convex in the free coord (a > 0); assert rather than
    # silently invert a non-PSD direction
    stopifnot(a > 0)
    beta <- b[perp] + 2 * (A[perp, k1] * X + A[perp, k2] * Y)
    gam <- sys$c[i] + b[k1] * X + b[k2] * Y +
      A[k1, k1] * X^2 + A[k2, k2] * Y^2 + 2 * A[k1, k2] * X * Y
    disc <- beta^2 - 4 * a * gam
    ok <- disc >= 0
    sq <- sqrt(pmax(disc, 0))
    lo_i <- ifelse(ok, (-beta - sq) / (2 * a), big) # empty constraint pushes
    hi_i <- ifelse(ok, (-beta + sq) / (2 * a), -big) # the interval apart
    L <- pmax(L, lo_i)
    H <- pmin(H, hi_i)
  }
  list(L = L, H = H, M = L - H)
}

# coordinate-matrix pair for a grid over axis-k1 values xg and axis-k2 values yg
region_grid <- function(xg, yg) {
  list(X = outer(xg, yg, function(a, b) a), Y = outer(xg, yg, function(a, b) b))
}
