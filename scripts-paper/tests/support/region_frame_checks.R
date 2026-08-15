# Checks for the 3D region figures' cube frames. Run from the package root:
#   Rscript scripts-paper/tests/support/region_frame_checks.R
# The hand-set ladders are constants while the frames around them come from each
# figure's own padded set box, so the cases that matter are the ones where the
# two disagree: neither draw_region_panes nor draw_region_axis clips, so a rung
# left outside the cube is drawn outside the cube.

source(file.path("scripts-paper", "config", "paths.R"))
# the auto frames key their per-figure adjustment off region_figure_tau_token,
# which the artifact manifest owns
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("mean_equation", "figures", "region_3d_frames.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

render <- PAPER_FIGURE_RENDER_CONTROL$region_3d

inside <- function(frame) {
  all(vapply(seq_along(frame$ticks), function(k) {
    at <- frame$ticks[[k]]
    all(at >= frame$lims[[k]][1] & at <= frame$lims[[k]][2])
  }, logical(1)))
}

# The published sd/tau = 0.05 figure without the OLS benchmark: nothing grows
# its free ends, so its padded box stops below the ladder's top rung on both
# axis 2 and axis 3. Those two rungs used to render past the corner, one of them
# landing on the axis 1 title.
narrow <- list(c(0.14, 0.30), c(-0.06, 0.02), c(-0.21, -0.13))
narrow_frame <- region_3d_frame(narrow, render, "baseline", NULL, "sd", 0.05)
check(
  "a baseline frame keeps every rung it draws inside the cube",
  inside(narrow_frame)
)
check(
  "the rungs above the box are the ones dropped",
  identical(
    lapply(narrow_frame$ticks, function(at) round(at, 2)),
    list(c(0.15, 0.20, 0.25), c(-0.05, 0), c(-0.20, -0.16))
  )
)

# The clip must not take rungs a figure can legitimately show: the same mode
# with a box wide enough for the whole ladder keeps it.
wide <- list(c(0.10, 0.30), c(-0.10, 0.10), c(-0.25, -0.10))
wide_frame <- region_3d_frame(wide, render, "baseline", NULL, "sd", 0.05)
check(
  "a baseline frame that contains its ladder keeps every rung",
  identical(wide_frame$ticks, render$ticks)
)

# The widest slack takes frame and ladder together, so the two agree already and
# the clip is a no-op there.
widest_frame <- region_3d_frame(wide, render, "widest", NULL, "sd", 0.30)
check(
  "the widest frame keeps its hand-set ladder intact",
  identical(widest_frame$ticks, render$widest_ticks)
)
check(
  "the widest frame draws its own hand-set limits",
  identical(widest_frame$lims, render$widest_limits)
)

# A frame that has drifted far enough to strand a ladder is a configuration
# error, not an axis with one label on it.
check(
  "a frame that strands a ladder is an error",
  inherits(
    tryCatch(
      region_3d_frame(
        list(c(0.14, 0.30), c(-0.06, 0.02), c(-0.30, -0.25)),
        render, "baseline", NULL, "sd", 0.05
      ),
      error = function(e) e
    ),
    "error"
  )
)

# The raw-unit baseline pair is read side by side, so its auto frame ignores the
# marked point rather than growing to pad it: both figures get one cube, one
# ladder and one set of labels.
shared_box <- list(c(0, 0.05), c(-0.12, 0.12), c(-0.94, -0.07))
check(
  "the raw-unit baseline pair draws the same frame with and without the OLS point",
  identical(
    region_3d_frame(shared_box, render, "auto", NULL, "b", 0.20),
    region_3d_frame(shared_box, render, "auto", c(0.02, 0, -0.18), "b", 0.20)
  )
)

.test$finish()
