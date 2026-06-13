# Constraint-checker closure membership probes (additive diagnostic).
# Built on the exported hetid closure API (make_system_checker /
# make_constraint_checker). The profile-bounds solver gives the identified-set
# interval WIDTHS; these helpers answer the complementary MEMBERSHIP question
# -- theta lies in the set iff max over constraints <= 0 (the hin <= 0
# convention) -- probed on a grid spanning the profile-bound box, plus the box
# center and its corners.

# Cartesian theta grid (one axis per component) over each component's
# [lower, upper] profile interval. An unbounded (+/-Inf) or failed (NA) side
# falls back to midpoint +/- fallback_span and the axis is flagged in fell_back.
make_theta_grid <- function(bounds_tbl, n_per_dim = 5L, fallback_span = 1) {
  n_comp <- nrow(bounds_tbl)
  fell_back <- logical(n_comp)
  axes <- vector("list", n_comp)
  lows <- numeric(n_comp)
  highs <- numeric(n_comp)
  for (k in seq_len(n_comp)) {
    lo <- bounds_tbl$lower[k]
    hi <- bounds_tbl$upper[k]
    if (!is.finite(lo) || !is.finite(hi)) {
      mid <- if (is.finite(lo)) lo else if (is.finite(hi)) hi else 0
      lo <- mid - fallback_span
      hi <- mid + fallback_span
      fell_back[k] <- TRUE
    }
    lows[k] <- lo
    highs[k] <- hi
    axes[[k]] <- seq(lo, hi, length.out = n_per_dim)
  }
  grid <- as.matrix(expand.grid(axes, KEEP.OUT.ATTRS = FALSE))
  corners <- as.matrix(
    expand.grid(
      Map(function(l, h) c(l, h), lows, highs),
      KEEP.OUT.ATTRS = FALSE
    )
  )
  dimnames(grid) <- NULL
  dimnames(corners) <- NULL
  list(
    grid = grid,
    corners = corners,
    center = (lows + highs) / 2,
    fell_back = fell_back
  )
}

# Probe identified-set membership for a quadratic system over the profile-bound
# box. Returns list(summary = one-row data frame, per_constraint = data frame).
probe_set_membership <- function(quadratic, bounds_tbl,
                                 n_per_dim = 5L, fallback_span = 1) {
  checker <- hetid::make_system_checker(quadratic)
  g <- make_theta_grid(bounds_tbl, n_per_dim, fallback_span)

  grid_vals <- apply(g$grid, 1, function(theta) checker(theta))
  if (is.null(dim(grid_vals))) {
    grid_vals <- matrix(grid_vals, nrow = 1)
  }
  grid_max <- apply(grid_vals, 2, max)
  inside <- grid_max <= 0
  n_grid <- ncol(grid_vals)
  n_inside <- sum(inside)

  center_vals <- checker(g$center)
  corner_max <- apply(g$corners, 1, function(theta) max(checker(theta)))

  constraint_names <- names(center_vals)
  if (is.null(constraint_names)) {
    constraint_names <- paste0("c", seq_len(nrow(grid_vals)))
  }

  # Binding constraint at an in-set point = the one attaining max(checker(theta))
  # (the value nearest the boundary from below).
  binding_freq <- rep(0, nrow(grid_vals))
  if (n_inside > 0) {
    binding_idx <- apply(grid_vals[, inside, drop = FALSE], 2, which.max)
    binding_freq <- tabulate(binding_idx, nbins = nrow(grid_vals)) / n_inside
  }

  # Explicit scalar-closure demonstration: the tightest constraint at the center,
  # evaluated directly through make_constraint_checker.
  tightest <- which.max(center_vals)
  scalar_check <- hetid::make_constraint_checker(
    quadratic$A_i[[tightest]], quadratic$b_i[[tightest]], quadratic$c_i[tightest]
  )

  summary <- data.frame(
    n_components = nrow(bounds_tbl),
    n_grid = n_grid,
    n_inside = n_inside,
    frac_inside = n_inside / n_grid,
    center_inside = max(center_vals) <= 0,
    n_corners = length(corner_max),
    n_corners_inside = sum(corner_max <= 0),
    any_fallback = any(g$fell_back),
    tightest_constraint = constraint_names[tightest],
    tightest_center_value = scalar_check(g$center),
    stringsAsFactors = FALSE
  )
  per_constraint <- data.frame(
    constraint = constraint_names,
    binding_freq = binding_freq,
    stringsAsFactors = FALSE
  )
  list(summary = summary, per_constraint = per_constraint)
}
