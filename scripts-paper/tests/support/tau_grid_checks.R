# Contract checks for the shared bounds-by-tau grid (config/tau_grid.R): shape,
# the preserved cap and backbone superset, the guards that keep a degenerate
# control from silently producing a smaller grid, and a static check that both
# call sites really route through the helper rather than rebuilding a grid.

paper_source_once(paper_path("config", "tau_grid.R"))

tg_control <- PAPER_FIGURE_RENDER_CONTROL$bounds_tau_grid
tg_backbone_n <- PAPER_ANALYSIS_CONTRACT$tau$figure_grid_n
# the shipped transition slack; dyadic, so the %.17g keys round-trip exactly
tg_tau_star <- 0.41455078125
tg_raises <- function(expr) inherits(tryCatch(expr, error = identity), "error")
tg_uniform <- function(tau_star) {
  backbone <- seq(0, tau_star, length.out = tg_backbone_n)
  backbone[backbone > 0 & backbone < tau_star]
}
tg_grid <- paper_bounds_tau_grid(tg_tau_star)
tg_base <- tg_uniform(tg_tau_star)

check(
  "the grid is strictly increasing and strictly inside (0, tau*)",
  all(diff(tg_grid) > 0) && all(tg_grid > 0) && all(tg_grid < tg_tau_star)
)
check(
  "every grid tau has a distinct full-precision lookup key",
  !anyDuplicated(vapply(tg_grid, paper_tau_key, character(1)))
)
check(
  "the maximum is bit-identical to the uniform backbone maximum",
  identical(max(tg_grid), max(tg_base))
)
check(
  "the grid is an exact superset of the uniform backbone",
  all(vapply(tg_base, function(tau) any(tg_grid == tau), logical(1)))
)
check(
  "the shipped controls yield the recorded grid cardinality",
  length(tg_grid) == 29L && length(tg_base) == 23L
)
check(
  "no grid tau collides with a display tau",
  !any(tg_grid %in% PAPER_ANALYSIS_CONTRACT$tau$display)
)

# the refinement has to buy resolution where the branch switch lives, so the
# tail count must separate the shipped subdivision from a weaker one
tg_coarse <- tg_control
tg_coarse$tail_subdivisions <- 2L
tg_tail_n <- function(grid) {
  sum(grid >= tg_control$tail_fraction * tg_tau_star)
}
check(
  "the refined tail carries at least six taus",
  tg_tail_n(tg_grid) >= 6L
)
check(
  "halving the subdivision drops the tail below that floor",
  tg_tail_n(paper_bounds_tau_grid(tg_tau_star, tg_coarse)) < 6L
)

# guards: a degenerate control must raise rather than return a shorter grid
tg_no_sub <- tg_control
tg_no_sub$tail_subdivisions <- NULL
check(
  "a control missing tail_subdivisions raises",
  tg_raises(paper_bounds_tau_grid(tg_tau_star, tg_no_sub))
)
tg_no_frac <- tg_control
tg_no_frac$tail_fraction <- NULL
check(
  "a control missing tail_fraction raises",
  tg_raises(paper_bounds_tau_grid(tg_tau_star, tg_no_frac))
)
tg_wide <- tg_control
tg_wide$tail_fraction <- 0.99
check(
  "a tail fraction too large for the backbone raises",
  tg_raises(paper_bounds_tau_grid(tg_tau_star, tg_wide, backbone_n = 10L))
)
check(
  "a non-scalar tau* raises",
  tg_raises(paper_bounds_tau_grid(c(0.2, 0.4)))
)
check(
  "a zero tau* raises",
  tg_raises(paper_bounds_tau_grid(0))
)
check(
  "a negative tau* raises",
  tg_raises(paper_bounds_tau_grid(-0.4))
)
check(
  "an NA tau* raises",
  tg_raises(paper_bounds_tau_grid(NA_real_))
)

# Static routing: comparing two calls of the helper to each other would be
# tautological, so read the call sites and assert they route through it and no
# longer build a grid of their own.
tg_call_sites <- c(
  paper_path("mean_equation", "inference", "compute_bounds_by_tau.R"),
  paper_path("log_variance", "figures", "render_bounds_by_tau.R")
)
tg_text <- vapply(
  tg_call_sites,
  function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
  character(1)
)
check(
  "both bounds-by-tau call sites route through the shared grid",
  all(grepl("paper_bounds_tau_grid(", tg_text, fixed = TRUE))
)
check(
  "neither call site still builds a grid from the backbone count",
  !any(grepl("figure_grid_n", tg_text, fixed = TRUE))
)

rm(
  tg_control, tg_backbone_n, tg_tau_star, tg_raises, tg_uniform, tg_grid,
  tg_base, tg_coarse, tg_tail_n, tg_no_sub, tg_no_frac, tg_wide,
  tg_call_sites, tg_text
)
