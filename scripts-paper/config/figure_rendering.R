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
  # Residual-diagnostic panels (figures/residual_diagnostics). One colour per
  # log-variance estimator, shared by the quantile-quantile and density figures
  # so a reader carries the key between them; the identified-set primary goes to
  # PPML because PPML is the baseline estimator. The reference curve and the
  # 45-degree line are the same neutral grey and the same dash, so neither reads
  # as a fourth estimator.
  residual_diagnostic = list(
    estimator_colors = c(
      "PPML" = "#2a78d6",
      "Harvey" = "#b3541e",
      "log-OLS" = "#6a3d9a"
    ),
    reference = "grey35",
    reference_linewidth = 0.35,
    reference_linetype = "22",
    point_size = 0.55,
    density_linewidth = 0.55,
    # wide enough that the three curves read as single-mode shapes rather than
    # kernel wobble; it flattens peak heights, so the figures are read for the
    # horizontal shift between estimators and not for the height of a mode
    density_adjust = 1.8,
    density_grid_n = 512L,
    # equal padding on both axes, which keeps the corner-to-corner reference
    # line exactly diagonal
    qq_pad_fraction = 0.03
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

# Render geometry and device sizes live alongside, and are asserted below with
# the style they configure.
paper_source_once(paper_path("config", "figure_render_control.R"))

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
