# Sensitivity of the rank-1 VFCI-baseline identified set to the slack tau.
# Locates tau* : the set is bounded for tau < tau* and unbounded for tau >= tau*.
# Two independent locators are reported and should agree:
#   (i)  empirical  -- total profile-bound width jumps to Inf (bisected)
#   (ii) analytic   -- recession metric min_||d||=1 max_i d' A_i(tau) d crosses 0

source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()

cli_h1("Tau sensitivity of the VFCI-baseline identified set")

base <- readRDS(file.path(
  OUTPUT_TEMP_DIR, "identification_baseline",
  "baseline_identification_results.rds"
))
moments <- base$moments
gamma <- base$gamma_baseline # rank-1 VFCI loadings
n_comp <- ncol(gamma)

quad_at_tau <- function(tau) {
  build_quadratic_system(gamma, rep(tau, n_comp), moments)$quadratic
}

# total identified-set width and boundedness at slack tau
width_at_tau <- function(tau) {
  b <- solve_all_profile_bounds(quad_at_tau(tau))
  list(
    total = sum(b$width),
    bounded = all(b$bounded_lower & b$bounded_upper),
    valid = all(b$valid_lower & b$valid_upper)
  )
}

# recession metric: <= 0 means a common non-positive-curvature direction exists
# (necessary for unboundedness of the quadratic intersection)
recession_metric <- function(tau, n_dir = 8000L) {
  a_list <- lapply(quad_at_tau(tau)$A_i, function(a) (a + t(a)) / 2)
  set.seed(1)
  dirs <- cbind(
    diag(n_comp), -diag(n_comp),
    matrix(rnorm(n_comp * n_dir), n_comp)
  )
  dirs <- sweep(dirs, 2, sqrt(colSums(dirs^2)), "/")
  vals <- apply(dirs, 2, function(d) {
    max(vapply(a_list, function(m) drop(t(d) %*% m %*% d), numeric(1)))
  })
  min(vals)
}

tau_grid <- seq(0, 0.2, by = 0.005)
rows <- lapply(tau_grid, function(t) {
  if (t == 0) {
    return(data.frame(
      tau = 0, total_width = 0, all_bounded = TRUE, recession = NA_real_
    ))
  }
  w <- width_at_tau(t)
  data.frame(
    tau = t, total_width = w$total, all_bounded = w$bounded,
    recession = recession_metric(t)
  )
})
sweep_df <- do.call(rbind, rows)

cli_h2("Width / boundedness across tau")
print(sweep_df, digits = 4)

# bisect tau* between the last bounded tau and the first unbounded tau
unb <- sweep_df$tau[!sweep_df$all_bounded]
if (length(unb) == 0) {
  tau_star <- NA_real_
  cli_alert_info("Bounded across the whole grid; tau* > {max(tau_grid)}")
} else {
  hi <- min(unb)
  lo <- max(sweep_df$tau[sweep_df$all_bounded & sweep_df$tau < hi])
  for (k in seq_len(40)) {
    mid <- (lo + hi) / 2
    if (width_at_tau(mid)$bounded) lo <- mid else hi <- mid
  }
  tau_star <- (lo + hi) / 2
  cli_alert_success("tau* ~= {.val {round(tau_star, 5)}} (bounded below, unbounded above)")
}

# save CSV + plots
out_dir <- file.path(OUTPUT_TEMP_DIR, "identification_optimized")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(sweep_df, file.path(out_dir, "tau_sensitivity.csv"), row.names = FALSE)

plot_df <- sweep_df[is.finite(sweep_df$total_width), ]
p_w <- ggplot(plot_df, aes(tau, total_width)) +
  geom_line(color = "#2166AC") +
  geom_point(color = "#2166AC", size = 1.5) +
  {
    if (!is.na(tau_star)) {
      geom_vline(xintercept = tau_star, linetype = "dashed", color = "#B2182B")
    }
  } +
  labs(
    title = "VFCI-baseline identified-set width vs. slack tau",
    subtitle = if (!is.na(tau_star)) {
      sprintf("Unbounded for tau >= tau* ~ %.4f (dashed)", tau_star)
    } else {
      "Bounded across the swept range"
    },
    x = expression(tau), y = "Total profile-bound width (finite only)"
  ) +
  theme_minimal()
ggsave(file.path(out_dir, "tau_sensitivity.png"), p_w,
  width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI
)
ggsave(file.path(out_dir, "tau_sensitivity.svg"), p_w,
  width = PLOT_WIDTH, height = PLOT_HEIGHT
)

cli_alert_success("Saved tau_sensitivity CSV/PNG/SVG to {.path {out_dir}}")
saveRDS(
  list(sweep = sweep_df, tau_star = tau_star),
  file.path(OUTPUT_TEMP_DIR, "identification_optimized", "tau_sensitivity.rds")
)
