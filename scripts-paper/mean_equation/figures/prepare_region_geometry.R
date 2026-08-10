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
paper_source_once(paper_path(
  "mean_equation", "figures", "region_envelope_non_convex.R"
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
#
# Each constraint is a quadratic in the free coordinate t with leading term
# a = A_i[perp, perp]. Convex constraints (a > 0) keep the interval between
# their roots; the joint feasible set is L = max_i lo_i, H = min_i hi_i --
# exact and vectorized, no optimizer. A single A_i has at most one positive
# eigenvalue for any gamma (rank-one PSD minus PSD; docs/lewbel_multivariate_
# set_identification.tex), so a <= 0 is the generic case along axes its one
# positive direction misses, more likely as tau grows -- not a fluke. Non-
# convex constraints instead EXCLUDE an interval (or nothing, if their
# parabola never crosses zero); a second pass intersects the running [L, H]
# with each exclusion's complement, still raising on a genuine two-piece split.
region_envelope <- function(sys, perp, X, Y) {
  dimension <- PAPER_ANALYSIS_CONTRACT$figure$region_dimension
  keep <- setdiff(seq_len(dimension), perp)
  k1 <- keep[1]
  k2 <- keep[2]
  big <- 1e6 # out-of-domain sentinel
  L <- matrix(-Inf, nrow(X), ncol(X))
  H <- matrix(Inf, nrow(X), ncol(X))
  non_convex <- integer(0)
  for (i in seq_along(sys$A)) {
    root <- region_quadratic_roots(sys, i, perp, k1, k2, X, Y)
    if (!isTRUE(root$a > 0)) {
      non_convex <- c(non_convex, i)
      next
    }
    ok <- root$disc >= 0
    lo_i <- ifelse(ok, root$lo, big) # empty constraint pushes
    hi_i <- ifelse(ok, root$hi, -big) # the interval apart
    L <- pmax(L, lo_i)
    H <- pmin(H, hi_i)
  }
  for (i in non_convex) {
    root <- region_quadratic_roots(sys, i, perp, k1, k2, X, Y)
    # a real exclusion interval that actually overlaps the running [L, H];
    # one that sits entirely outside it (no real roots, or roots off to one
    # side) changes nothing, so it must not fall through to any branch below
    excludes <- root$disc > 0 & root$lo < H & root$hi > L
    covers_all <- excludes & root$lo <= L & root$hi >= H
    trims_low <- excludes & !covers_all & root$lo <= L
    trims_high <- excludes & !covers_all & !trims_low & root$hi >= H
    splits <- excludes & !covers_all & !trims_low & !trims_high
    if (any(splits, na.rm = TRUE)) {
      stop(region_non_convex_error(i, perp, root$a))
    }
    L <- ifelse(covers_all, big, ifelse(trims_low, root$hi, L))
    H <- ifelse(covers_all, -big, ifelse(trims_high, root$lo, H))
  }
  list(L = L, H = H, M = L - H)
}

# coordinate-matrix pair for a grid over axis-k1 values xg and axis-k2 values yg
region_grid <- function(xg, yg) {
  list(X = outer(xg, yg, function(a, b) a), Y = outer(xg, yg, function(a, b) b))
}
