# Versioned publication style and render geometry for paper figures.

PAPER_FIGURE_STYLE <- list(
  version = "1.1.0",
  identified_set = list(
    primary = "#2a78d6",
    reference = "grey35",
    ribbon_alpha = 0.35,
    boundary_linewidth = 0.4,
    reference_linewidth = 0.35,
    # anchors of the sequential ramp for nested slack bands, darkest first, so
    # a tighter tau reads as a deeper blue; interpolated to the sweep length
    sweep_ramp = c("#0b3d91", "#2a78d6", "#bcd9f5")
  ),
  log_variance = list(
    one_sided = "#b3541e",
    point = "#b2182b",
    unreliable = "#c23b22",
    unbounded = "grey55",
    one_sided_point_size = 1.6,
    sampled_point_size = 0.55,
    unsampled_shade = 0.2,
    point_linewidth = 0.55
  ),
  variance_bound = list(
    series_colors = c(
      "SDF news" = "#B01513",
      "Expected SDF" = "#7F7F7F"
    ),
    line_width = 0.8,
    point_size = 1.6
  ),
  descriptive = list(
    group_colors = c(
      "consumption growth" = "#2a78d6",
      "expected SDF PCs" = "#1baf7a",
      "SDF news PCs" = "#eda100",
      "expected SDF" = "#008300",
      "SDF news" = "#4a3aa7",
      "lagged expected SDF PCs" = "#e34948",
      "yield vols" = "#c2439c"
    )
  ),
  # Identified-region figures (render_region_3d.R, render_projections.R). The
  # tau = 0 point marker is one crimson shared by both figures, and the OLS
  # benchmark marker of their _ols variants is one orange diamond shared too.
  region = list(
    tau0_point = "#dc143c",
    ols_point = "#ff8c00",
    ols_pch = 23L,
    wall_fill = "#9dc3e6",
    face_fill = "#4a90d9",
    mesh_segment = "#112233"
  )
)

PAPER_FIGURE_RENDER_CONTROL <- list(
  version = "1.0.0",
  devices = list(
    mean_bounds = c(width = 10, height = 5.5),
    logvar_bounds = c(width = 10, height = 6.5),
    fitted_volatility = c(width = 10, height = 6.25),
    # Published combined slack panel. Much wider than the paper's shared 5.5in
    # canvas because this figure centres its PANEL rather than its canvas, which
    # costs the panel the width of the axis block twice over. At 7in = 504pt it
    # runs about 17pt past each side of the 469.755pt \textwidth, so LaTeX logs
    # one overfull hbox for it and \centering spreads the overhang evenly into
    # the 72pt margins -- deliberate, author asked for the width. Height fixed.
    fitted_volatility_sweep = c(width = 7.0, height = 5.5 / 1.618),
    variance_bounds = c(width = 5.5, height = 5.5 / 1.618),
    descriptive = c(width = 11, height = 8.5)
  ),
  # shape of the shared bounds-by-tau grid (tau_grid.R): the historical uniform
  # backbone with its tail intervals subdivided, so the branch switch near tau*
  # renders as a knee rather than one chord
  bounds_tau_grid = list(
    tail_fraction = 0.9,
    tail_subdivisions = 4L
  ),
  region_3d = list(
    seed = 15599L,
    wall_grid_points = 440L,
    camera = list(
      elevation = 23.1,
      azimuth = 152.8,
      azimuth_offset = 90,
      radius = 14.8,
      distance = 1,
      expand = 0.75
    ),
    limit_padding = 0.25,
    # Hand-set cube frames and their ladders. The baseline overrides two
    # endpoints of its padded box and uses ticks; the widest slack outgrows both,
    # so it takes widest_limits and widest_ticks outright. Every frame must
    # contain its own set box and the OLS point, which render_region_3d.R
    # asserts, and every tick must fall inside its own frame.
    manual_limits = list(x_upper = 0.28, y_lower = -0.08),
    widest_limits = list(
      c(-0.16, 0.73),
      c(-0.35, 0.35),
      c(-0.38466, 0.02)
    ),
    widest_ticks = list(
      c(0, 0.25, 0.50),
      c(-0.20, 0, 0.20),
      c(-0.30, -0.15, 0)
    ),
    ticks = list(
      c(0.15, 0.20, 0.25),
      c(-0.05, 0, 0.05),
      c(-0.20, -0.16, -0.12)
    ),
    tick_digits = 2L,
    # Ladder density for the slacks and unit systems without a hand-set frame.
    # Those frames also set their own decimal count from the ladder they get:
    # raw b_{1,N} ticks land on thousandths, which tick_digits would collapse.
    auto_tick_n = 3L,
    # Axis titles per unit system. "sd" scales each coefficient by its news-PC
    # standard deviation; "b" plots the coefficient itself.
    axis_labels = list(
      sd = c(
        "$\\sigma(PC_{1,N})\\, b_{1,N}$",
        "$\\sigma(PC_{2,N})\\, b_{2,N}$",
        "$\\sigma(PC_{3,N})\\, b_{3,N}$"
      ),
      b = c("$b_{1,N}$", "$b_{2,N}$", "$b_{3,N}$")
    ),
    device = list(
      width = 9.6,
      height = 8.4,
      family = "DejaVu Sans"
    )
  ),
  projections = list(
    grid_points = 300L,
    tau_colors = c("#472B7A", "#26818E", "#5DC863"),
    range_padding = 0.06,
    device = c(width = 11, height = 4.7),
    # one row of panels; the legend overlays the strip they leave above their
    # square boxes rather than taking a layout row of its own
    layout = matrix(c(1L, 2L, 3L), nrow = 1L)
  )
)

stopifnot(
  nzchar(PAPER_FIGURE_STYLE$version),
  nzchar(PAPER_FIGURE_RENDER_CONTROL$version),
  PAPER_FIGURE_STYLE$identified_set$ribbon_alpha > 0,
  PAPER_FIGURE_STYLE$identified_set$ribbon_alpha < 1,
  all(vapply(
    PAPER_FIGURE_RENDER_CONTROL$devices,
    function(device) all(device > 0),
    logical(1)
  )),
  PAPER_FIGURE_RENDER_CONTROL$region_3d$wall_grid_points >= 2L,
  length(PAPER_FIGURE_RENDER_CONTROL$region_3d$ticks) == 3L,
  PAPER_FIGURE_RENDER_CONTROL$projections$grid_points >= 2L
)
