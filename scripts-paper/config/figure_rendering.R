# Versioned publication style and render geometry for paper figures.

PAPER_FIGURE_STYLE <- list(
  version = "1.1.0",
  identified_set = list(
    primary = "#2a78d6",
    reference = "grey35",
    ribbon_alpha = 0.35,
    boundary_linewidth = 0.4,
    reference_linewidth = 0.35
  ),
  log_variance = list(
    one_sided = "#b3541e",
    point = "#b2182b",
    unreliable = "#c23b22",
    unbounded = "grey55",
    one_sided_point_size = 1.6,
    point_linewidth = 0.55
  ),
  variance_bound = list(
    series_colors = c(
      "SDF news" = "#4a3aa7",
      "expected SDF" = "#008300"
    ),
    line_width = 1.2,
    point_size = 3
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
  # tau = 0 point marker is one crimson shared by both figures.
  region = list(
    tau0_point = "#dc143c",
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
    variance_bounds = c(width = 10, height = 6),
    descriptive = c(width = 11, height = 8.5)
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
    manual_limits = list(x_upper = 0.28, y_lower = -0.08),
    ticks = list(
      c(0.15, 0.20, 0.25),
      c(-0.05, 0, 0.05),
      c(-0.20, -0.16, -0.12)
    ),
    tick_digits = 2L,
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
    layout = matrix(
      c(1L, 2L, 3L, 4L, 4L, 4L),
      nrow = 2L,
      byrow = TRUE
    ),
    layout_heights = c(6, 1)
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
