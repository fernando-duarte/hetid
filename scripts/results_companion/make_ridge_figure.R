# Headline figure for the results companion: the identified set for the 5- and
# 9-year SDF-news loadings, projected onto the (theta_5y, theta_9y) plane (the
# 2-year coordinate profiled out). Overlays two slacks to show the set stretching
# along the near-collinear ridge as tau rises from 0.02 to the baseline 0.05.
# Deterministic (seed 123).

source(here::here("scripts/utils/common_settings.R"))
suppressMessages(library(ggplot2))
set.seed(SEED)
fig_dir <- file.path(OUTPUT_PAPER_DIR, "identification")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

inputs <- load_identification_inputs()
resid <- compute_identification_residuals(inputs$data)
moments <- compute_identification_moments(resid$w1, resid$w2, resid$pcs_aligned)
gamma_vfci <- get_baseline_gamma(
  "vfci",
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS, n_components = 3
)
pt0 <- solve_point_identification(
  build_pipeline_quadratic_system(gamma_vfci, rep(0, 3), moments)$components
)$theta

# Feasibility of (theta_2, theta_5, theta_9) over the grid, profiling theta_2 by a
# scan: a (theta_5, theta_9) point is in the projection iff some theta_2 satisfies
# all three quadratic constraints g_i(theta) = theta'A_i theta + b_i'theta + c_i <= 0.
project <- function(tau, th5, th9, th2_scan) {
  qs <- build_pipeline_quadratic_system(gamma_vfci, rep(tau, 3), moments)$quadratic
  g5 <- rep(th5, times = length(th9))
  g9 <- rep(th9, each = length(th5))
  feas <- rep(FALSE, length(g5))
  for (t2 in th2_scan) {
    ok <- rep(TRUE, length(g5))
    for (i in seq_len(3)) {
      a <- qs$A_i[[i]]
      b <- qs$b_i[[i]]
      gi <- a[1, 1] * t2^2 + a[2, 2] * g5^2 + a[3, 3] * g9^2 +
        2 * a[1, 2] * t2 * g5 + 2 * a[1, 3] * t2 * g9 + 2 * a[2, 3] * g5 * g9 +
        b[1] * t2 + b[2] * g5 + b[3] * g9 + qs$c_i[[i]]
      ok <- ok & (gi <= 0)
      if (!any(ok)) break
    }
    feas <- feas | ok
    if (all(feas)) break
  }
  data.frame(th5 = g5, th9 = g9, feasible = feas)
}

n_grid <- 190L
th5 <- seq(-110000, 20000, length.out = n_grid)
th9 <- seq(-20000, 185000, length.out = n_grid)
th2_scan <- seq(-13000, 2000, length.out = 1500L)
f02 <- project(0.02, th5, th9, th2_scan)$feasible
proj <- project(BASELINE_TAU, th5, th9, th2_scan)
proj$f02 <- f02
proj <- proj[proj$feasible, , drop = FALSE]
proj$region <- ifelse(proj$f02, "Slack tau = 0.02 (bounded)", "Extension at tau = 0.05")
proj$region <- factor(proj$region,
  levels = c("Slack tau = 0.02 (bounded)", "Extension at tau = 0.05")
)
cat("feasible tiles: tau<=0.02 =", sum(f02), " tau<=0.05 =", nrow(proj), "\n")

# Robustness guard: the hard-coded axes below are a deliberate viewing window on
# a set that stretches to infinity along the ridge, not a data-derived bounding
# box. If a future model/parameter shift moves the set entirely outside this
# window, `proj` would be empty and ggplot would silently emit a blank figure.
# Fail loudly instead so the stale axes get noticed and retuned.
if (nrow(proj) == 0L) {
  stop(
    "No feasible (theta_5y, theta_9y) tiles fall within the hard-coded axis ",
    "window (th5 in [", min(th5), ", ", max(th5), "], th9 in [", min(th9),
    ", ", max(th9), "]). The identified set has likely moved; retune the ",
    "`th5`/`th9`/`th2_scan` ranges before regenerating the ridge figure.",
    call. = FALSE
  )
}

point_df <- data.frame(th5 = pt0[2], th9 = pt0[3])
ttl <- paste(
  "Near-collinear bond news pins down one combination of the loadings;",
  "\nthe orthogonal direction stretches off to infinity as the slack rises"
)
p <- ggplot(proj, aes(th5 / 1000, th9 / 1000, fill = region)) +
  geom_raster() +
  scale_fill_manual(values = c(
    "Slack tau = 0.02 (bounded)" = "#08519c",
    "Extension at tau = 0.05" = "#9ecae1"
  ), name = NULL) +
  geom_point(
    data = point_df, aes(th5 / 1000, th9 / 1000),
    inherit.aes = FALSE, shape = 21, size = 2.6, fill = "white", color = "black", stroke = 0.7
  ) +
  annotate("text",
    x = pt0[2] / 1000 + 14, y = pt0[3] / 1000 - 16,
    label = "exact-restriction point (tau = 0)", size = 3, hjust = 0.5
  ) +
  labs(
    title = ttl,
    x = expression("Loading on 5-year SDF news," ~ theta[5][y] ~ "(thousands)"),
    y = expression("Loading on 9-year SDF news," ~ theta[9][y] ~ "(thousands)")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.title = element_text(size = 10.5, face = "plain"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "identified_set_ridge.pdf"), p, width = 7, height = 5.2)
ggsave(file.path(fig_dir, "identified_set_ridge.png"), p, width = 7, height = 5.2, dpi = 300)
cat("saved:", file.path(fig_dir, "identified_set_ridge.{pdf,png}"), "\n")
