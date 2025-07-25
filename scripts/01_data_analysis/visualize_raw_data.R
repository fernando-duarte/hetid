# Visualize Raw Data
# Create exploratory plots of raw data

library(hetid)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(corrplot)
library(svglite)
library(here)
source(here::here("scripts/utils/common_settings.R"))

input_path <- file.path(OUTPUT_DIR, "temp/data.rds")
data <- readRDS(input_path)
df <- as.data.frame(data)

plot_dir <- file.path(OUTPUT_DIR, "temp/plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

colors <- c("darkblue", "darkred", "darkgreen", "darkorange", "purple", "brown")

# Time Series Plot of Yields
yield_vars <- grep("^y\\d+$", names(df), value = TRUE)
yields_long <- df %>%
  select(date, all_of(yield_vars)) %>%
  pivot_longer(cols = -date, names_to = "maturity", values_to = "yield") %>%
  mutate(maturity = as.numeric(gsub("y", "", maturity)))

p_yields <- ggplot(yields_long, aes(x = date, y = yield, color = factor(maturity))) +
  geom_line() +
  scale_color_viridis_d(name = "Maturity") +
  labs(
    title = "Treasury Yields Over Time",
    x = "Date",
    y = "Yield (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

display_and_save_plot(p_yields, "yields_time_series.svg", dir = plot_dir)

# Term Structure Snapshots
snapshot_dates <- c(
  min(df$date),
  df$date[which.min(abs(df$date - as.Date("2008-12-31")))], # Financial crisis
  df$date[which.min(abs(df$date - as.Date("2020-03-31")))], # COVID
  max(df$date)
)

snapshot_data <- df %>%
  filter(date %in% snapshot_dates) %>%
  select(date, all_of(yield_vars)) %>%
  pivot_longer(cols = -date, names_to = "maturity", values_to = "yield") %>%
  mutate(
    maturity = as.numeric(gsub("y", "", maturity)),
    date_label = format(date, "%Y-%m")
  )

p_term_structure <- ggplot(snapshot_data, aes(
  x = maturity, y = yield,
  color = date_label, group = date_label
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Term Structure Snapshots",
    x = "Maturity (years)",
    y = "Yield (%)",
    color = "Date"
  ) +
  theme_minimal()

display_and_save_plot(p_term_structure, "term_structure_snapshots.svg", dir = plot_dir)

# Term Premia Time Series
tp_vars <- grep("^tp\\d+$", names(df), value = TRUE)
tp_long <- df %>%
  select(date, all_of(tp_vars)) %>%
  pivot_longer(cols = -date, names_to = "maturity", values_to = "term_premium") %>%
  mutate(maturity = as.numeric(gsub("tp", "", maturity)))

p_tp <- ggplot(
  tp_long %>% filter(maturity %in% c(2, 5, 10)),
  aes(x = date, y = term_premium, color = factor(maturity))
) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = colors[1:3], name = "Maturity") +
  labs(
    title = "Term Premia Over Time",
    x = "Date",
    y = "Term Premium (%)"
  ) +
  theme_minimal()

display_and_save_plot(p_tp, "term_premia_time_series.svg", dir = plot_dir)

# Lagged Principal Components Time Series
pc_lag_vars <- grep("^pc\\d+_lag1$", names(df), value = TRUE)[1:3] # First 3 lagged PCs
pc_lag_long <- df %>%
  select(date, all_of(pc_lag_vars)) %>%
  pivot_longer(cols = -date, names_to = "pc", values_to = "value")

p_pcs <- ggplot(pc_lag_long, aes(x = date, y = value)) +
  geom_line(color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~pc, ncol = 1, scales = "free_y") +
  labs(
    title = "Lagged Principal Components Over Time",
    x = "Date",
    y = "Value (Normalized)"
  ) +
  theme_minimal()

display_and_save_plot(p_pcs, "lagged_principal_components.svg", dir = plot_dir, height = PLOT_HEIGHT * 1.2)

# Consumption Growth
p_consumption <- ggplot(df, aes(x = date, y = !!sym(HETID_CONSTANTS$CONSUMPTION_GROWTH_COL))) +
  geom_line(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  labs(
    title = "Consumption Growth Over Time",
    x = "Date",
    y = "Growth Rate (%)"
  ) +
  theme_minimal()

display_and_save_plot(p_consumption, "consumption_growth.svg", dir = plot_dir, height = PLOT_HEIGHT * 0.7)

# Correlation Heatmaps
pc_lag_vars_all <- grep("^pc\\d+_lag1$", names(df), value = TRUE)

# Yields correlation
cor_yields <- cor(df[yield_vars], use = "complete.obs")
save_correlation_heatmap(cor_yields, "Yield Correlations", "correlation_yields.svg", dir = plot_dir)

# Term premia correlation
cor_tp <- cor(df[tp_vars], use = "complete.obs")
save_correlation_heatmap(cor_tp, "Term Premia Correlations", "correlation_term_premia.svg", dir = plot_dir)

# Lagged PCs correlation
cor_pc_lag <- cor(df[pc_lag_vars_all], use = "complete.obs")
save_correlation_heatmap(cor_pc_lag, "Lagged PC Correlations", "correlation_lagged_pcs.svg", dir = plot_dir)

# Cross-correlation: Lagged PCs vs Yields
cor_pc_yields <- cor(df[pc_lag_vars_all], df[yield_vars], use = "complete.obs")
save_correlation_heatmap(cor_pc_yields, "Correlations: Lagged PCs vs Yields", "correlation_pcs_yields.svg", dir = plot_dir)

# Cross-correlation: Lagged PCs vs Term Premia
cor_pc_tp <- cor(df[pc_lag_vars_all], df[tp_vars], use = "complete.obs")
save_correlation_heatmap(cor_pc_tp, "Correlations: Lagged PCs vs Term Premia", "correlation_pcs_tp.svg", dir = plot_dir)

# Distribution Plots
yield_dist_data <- df %>%
  select(all_of(c("y2", "y5", "y10"))) %>%
  pivot_longer(everything(), names_to = "maturity", values_to = "yield")

p_yield_dist <- ggplot(yield_dist_data, aes(x = yield, fill = maturity)) +
  geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
  facet_wrap(~maturity, scales = "free") +
  labs(
    title = "Distribution of Selected Yields",
    x = "Yield (%)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

display_and_save_plot(p_yield_dist, "yield_distributions.svg", dir = plot_dir)

# Lagged PC distributions
pc_lag_dist_vars <- pc_lag_vars_all[1:3] # First 3 lagged PCs
pc_dist_data <- df %>%
  select(all_of(pc_lag_dist_vars)) %>%
  pivot_longer(everything(), names_to = "pc", values_to = "value")

p_pc_dist <- ggplot(pc_dist_data, aes(x = value, fill = pc)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~pc, scales = "free") +
  labs(
    title = "Distribution of Lagged Principal Components",
    x = "Value (Normalized)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

display_and_save_plot(p_pc_dist, "lagged_pc_distributions.svg", dir = plot_dir)

# Yield Curve Slope (10Y - 2Y)
df$slope_10_2 <- df$y10 - df$y2

p_slope <- ggplot(df, aes(x = date, y = slope_10_2)) +
  geom_line(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = pmin(slope_10_2, 0), ymax = 0),
    fill = "red", alpha = 0.2
  ) +
  labs(
    title = "Yield Curve Slope (10Y - 2Y)",
    x = "Date",
    y = "Slope (%)"
  ) +
  theme_minimal()

display_and_save_plot(p_slope, "yield_curve_slope.svg", dir = plot_dir, height = PLOT_HEIGHT * 0.7)

# Scatterplot Matrix for Key Variables
key_vars <- c("y2", "y10", "tp2", "tp10", "pc1_lag1", HETID_CONSTANTS$CONSUMPTION_GROWTH_COL)
pairs_data <- df[key_vars]

svglite(file.path(plot_dir, "scatterplot_matrix.svg"), width = 10, height = 10)
pairs(pairs_data,
  main = "Scatterplot Matrix of Key Variables",
  pch = 19, cex = 0.5, col = rgb(0, 0, 1, 0.3)
)
dev.off()

# Rolling Statistics
window_size <- 20 # 5 years for quarterly data

# Note: This will create NA values for the first (window_size - 1) observations
df$y2_roll_mean <- zoo::rollmean(df$y2, window_size, fill = NA, align = "right")
df$y2_roll_sd <- zoo::rollapply(df$y2, window_size, sd, fill = NA, align = "right")

p_rolling <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = y2), color = "gray70", alpha = 0.7) +
  geom_line(aes(y = y2_roll_mean), color = "darkblue", linewidth = 1) +
  geom_ribbon(
    aes(
      ymin = y2_roll_mean - 2 * y2_roll_sd,
      ymax = y2_roll_mean + 2 * y2_roll_sd
    ),
    alpha = 0.2, fill = "blue"
  ) +
  labs(
    title = "2-Year Yield: Rolling Mean and 2-SD Band (5-year window)",
    x = "Date",
    y = "Yield (%)"
  ) +
  theme_minimal()

display_and_save_plot(p_rolling, "rolling_statistics.svg", dir = plot_dir, height = PLOT_HEIGHT * 0.7)

cli_h1("Exploratory Plots Generated")
cli_ul(c(
  "yields_time_series.svg",
  "term_structure_snapshots.svg",
  "term_premia_time_series.svg",
  "lagged_principal_components.svg",
  "consumption_growth.svg",
  "correlation_yields.svg",
  "correlation_term_premia.svg",
  "correlation_lagged_pcs.svg",
  "correlation_pcs_yields.svg",
  "correlation_pcs_tp.svg",
  "yield_distributions.svg",
  "lagged_pc_distributions.svg",
  "yield_curve_slope.svg",
  "scatterplot_matrix.svg",
  "rolling_statistics.svg"
))
