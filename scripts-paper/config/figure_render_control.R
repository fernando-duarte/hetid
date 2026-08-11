# Render geometry and per-figure device control for paper figures. Split from
# figure_rendering.R for the repository line cap: that file owns the visual
# style (colours, widths, sizes) and this one owns how each figure is laid out
# and how large its canvas is. Sourced by figure_rendering.R, which validates
# both objects together, so nothing sources this file directly.

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
    # square, because the quantile-quantile panel puts the same range on both
    # axes and a non-square frame would tilt its 45-degree reference line
    residual_qq = c(width = 5.5, height = 5.5),
    residual_density = c(width = 5.5, height = 5.5 / 1.618),
    descriptive = c(width = 11, height = 8.5)
  ),
  # shape of the shared bounds-by-tau grid (tau_grid.R): the historical uniform
  # backbone with its tail intervals subdivided, so the branch switch near tau*
  # renders as a knee rather than one chord
  bounds_tau_grid = list(
    tail_fraction = 0.9,
    tail_subdivisions = 4L
  ),
  # The bounds-by-tau panels cap their x axis at the sampled tau where this grid
  # starts subdividing; the cap is derived from the grid's own spacing in
  # bounds_by_tau_plot.R, so it needs no knob here and cannot drift from the
  # values above. It is a coord_cartesian zoom and changes no tau, no bound and
  # no tau*.
  region_3d = list(
    # Slacks drawn by the region figures, in both unit systems and with and
    # without the OLS benchmark; artifact_manifest_region.R turns this vector
    # into the figure records, so a slack with no record cannot render. It lives
    # here rather than beside the other tau vectors in the analysis contract
    # because that file is one of BOOTSTRAP_STAGE_CODE_FILES: editing it digests
    # differently and discards the cached bootstrap draws, and which slacks get
    # drawn has no bearing on those draws.
    taus = c(0.05, 0.10, 0.20, 0.30),
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
    # Per-figure nudges to an auto frame, keyed by unit system and slack token.
    # x_lower and x_upper pin axis 1's ends; y_lower_drop lowers axis 2's bottom
    # by that much. Applied before the OLS growth and the tick ladder, so the
    # _ols variant can still widen past a pin it would otherwise fall outside.
    auto_frame_adjust = list(
      b = list(tau0p2 = list(
        x_lower = -0.01, x_upper = 0.06, y_lower_drop = 0.025
      ))
    ),
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
    # Ladder step pinned per unit system and coefficient, overriding the step
    # pretty() would pick; NA leaves that coefficient automatic. b_{2,N} reads
    # better on its three round tenths than on the five rungs pretty() returns
    # for its range. Only the per-coefficient frames consult this, so the SD
    # pair has no entry.
    tick_steps = list(b = c(NA, 0.1, NA)),
    grid_points = 300L,
    tau_colors = c("#472B7A", "#26818E", "#5DC863"),
    range_padding = 0.06,
    device = c(width = 11, height = 4.7),
    # one row of panels; the legend overlays the strip they leave above their
    # square boxes rather than taking a layout row of its own
    layout = matrix(c(1L, 2L, 3L), nrow = 1L)
  )
)
