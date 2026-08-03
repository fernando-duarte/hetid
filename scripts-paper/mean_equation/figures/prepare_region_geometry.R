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

paper_source_once(paper_path("support", "identification", "api.R"))
paper_source_once(paper_path("support", "identification", "tau_star.R"))
# solving a region slack off the published display grid needs the profile
# solver behind coef_interval_tables, so this module declares it rather than
# inheriting it from whichever earlier stage happened to load it
paper_source_once(paper_path(
  "support", "identification", "profile_solver_core.R"
))
paper_source_once(paper_path(
  "support", "identification", "profile_bounds_api.R"
))
paper_source_once(paper_path(
  "mean_equation", "inference", "refine_bounds_by_tau.R"
))

# news-PC standard deviations that define the SD-unit axes. Scale on Y2, which
# is PC_N itself: the published mean spec does not impose the beta2R = 0 null,
# so w2 = resid(lm(Y2 ~ X)) and sd(w2[, i]) is not sd(PC_{N,i})
region_sd <- apply(set_id_mean_eq$y2, 2, stats::sd)

# Axis scale of one unit system. Every helper below takes the scale as an
# argument and defaults to the SD axes, so the projection figures keep reading
# the standardized geometry while the region figures also ask for raw b_{k,N}.
region_axis_scale <- function(units = REGION_FIGURE_UNITS) {
  units <- match.arg(units)
  if (identical(units, "sd")) {
    region_sd
  } else {
    rep(1, PAPER_ANALYSIS_CONTRACT$figure$region_dimension)
  }
}

# quadratic system {A_i, b_i, c_i} at a given slack tau, on the requested axes
region_sd_system <- function(tau, s = region_sd) {
  qs <- tau_quadratic_system(set_id_mean_eq$gamma, tau, set_id_mean_eq$moments)
  list(
    A = lapply(qs$A_i, function(m) m / outer(s, s)),
    b = lapply(qs$b_i, function(v) v / s),
    c = as.numeric(unlist(qs$c_i))
  )
}

# Warm-refined theta box at one slack. A display slack is read straight off the
# published set_tables; a region slack off that grid is solved here from the
# same warm chain those tables use, exactly as the fitted-volatility sweep does
# for its own off-grid slacks. The chain is walked once and memoized, because it
# is shared by every unit system and both OLS variants of a given slack.
region_theta_box <- local({
  chain <- NULL
  function(tau) {
    published <- set_id_mean_eq$set_tables[[paper_tau_key(tau)]]
    if (!is.null(published)) {
      return(published$theta)
    }
    if (is.null(chain)) {
      chain <<- set_id_display_tau_refinement(
        sort(unique(c(
          set_id_mean_eq$tau_display, PAPER_FIGURE_RENDER_CONTROL$region_3d$taus
        ))),
        set_id_mean_eq$theta_table$point,
        set_id_mean_eq$gamma, set_id_mean_eq$moments,
        set_id_mean_eq$beta1r, set_id_mean_eq$beta2r
      )
    }
    box <- chain[[paper_tau_key(tau)]]
    stopifnot(!is.null(box))
    box
  }
})

# per-coefficient bounding box of the set at slack tau, on the requested axes
region_sd_box <- function(tau, s = region_sd) {
  theta <- region_theta_box(tau)
  list(lo = theta$set_lower * s, hi = theta$set_upper * s)
}

# tau = 0 point
region_sd_point <- function(s = region_sd) set_id_mean_eq$theta_table$point * s

# OLS benchmark point (Y2 treated as exogenous, same sample)
region_sd_ols_point <- function(s = region_sd) {
  set_id_mean_eq$theta_table$ols * s
}

# Closed-form free-coordinate envelope over an (k1, k2) grid (matrices X, Y for
# the two kept axes, k1 < k2); perp is the projected-out axis. Returns the
# bottom skin L, top skin H, and margin M = L - H (<= 0 inside the projection).
region_envelope <- function(sys, perp, X, Y) {
  dimension <- PAPER_ANALYSIS_CONTRACT$figure$region_dimension
  keep <- setdiff(seq_len(dimension), perp)
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
