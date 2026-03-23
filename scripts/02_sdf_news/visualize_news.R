# Visualize Price News and SDF News
# Create exploratory plots for bond price news and SDF innovations

source(here::here("scripts/utils/common_settings.R"))
# Core packages (hetid, dplyr, tidyr, gt, DT, here, cli) loaded via common_settings.R

# Load specialized packages for this script
load_visualization_packages() # ggplot2, gridExtra, plotly
library(lubridate) # Date handling
library(zoo) # Time series utilities
library(corrplot) # Correlation plots
library(viridis) # Color palettes

cli_h1("Visualizing Price News and SDF Innovations")

# Load data and outputs
data_with_news <- readRDS(file.path(OUTPUT_DIR, "temp/sdf_news/data_with_news.rds"))
compute_output <- readRDS(file.path(OUTPUT_DIR, "temp/sdf_news/compute_output.rds"))
analysis_results <- readRDS(file.path(OUTPUT_DIR, "temp/sdf_news/comprehensive_analysis.rds"))

# Extract variables
price_news_vars <- compute_output$price_news_vars
pc_lag_vars <- grep("^pc\\d+_lag1$", names(data_with_news), value = TRUE)

# Create output directory for plots
plot_dir <- file.path(OUTPUT_DIR, "temp/sdf_news/exploratory_plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

cli_h1("1. TIME SERIES PLOTS")

# Extract price news matrix
price_news <- as.matrix(data_with_news[, price_news_vars])

# Create long format data for plotting
price_news_long <- data_with_news |>
  select(date, all_of(price_news_vars)) |>
  pivot_longer(cols = -date, names_to = "maturity", values_to = "price_news") |>
  mutate(
    maturity_num = as.numeric(gsub("price_news_", "", maturity)),
    maturity_label = paste0(maturity_num, "-year")
  )

cli_h2("Multi-Panel Time Series Plot")

p_ts_multi <- ggplot(price_news_long, aes(x = date, y = price_news)) +
  geom_line(color = "darkblue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  facet_wrap(~maturity_label, scales = "free_y", ncol = 2) +
  labs(
    title = "Price News Across Different Maturities",
    subtitle = "Quarterly data with zero reference line",
    x = "Date",
    y = "Price News"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    panel.spacing = unit(1, "lines")
  )

ggsave(file.path(plot_dir, "price_news_multi_panel.svg"), p_ts_multi, width = 12, height = 10)
cli_alert_success("Saved multi-panel time series plot")

cli_h2("Overlaid Time Series Plot")

# Select a subset of maturities for clarity
# Use first, middle, and last available maturities
n_maturities <- length(price_news_vars)
if (n_maturities >= 3) {
  selected_indices <- c(1, ceiling(n_maturities / 2), n_maturities)
} else {
  selected_indices <- seq_len(n_maturities)
}
selected_maturities <- price_news_vars[selected_indices]
if (length(selected_maturities) > 0) {
  p_ts_overlay <- ggplot(
    price_news_long |> filter(maturity %in% selected_maturities),
    aes(x = date, y = price_news, color = maturity_label)
  ) +
    geom_line(alpha = 0.8, linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_color_viridis_d() +
    labs(
      title = "Price News Over Time: Selected Maturities",
      x = "Date",
      y = "Price News",
      color = "Maturity"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave(file.path(plot_dir, "price_news_overlay.svg"), p_ts_overlay, width = 10, height = 6)
}

cli_h1("2. DISTRIBUTION PLOTS")

cli_h2("Density Plots by Maturity")

p_density <- ggplot(price_news_long, aes(x = price_news, fill = maturity_label)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~maturity_label, scales = "free", ncol = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_fill_viridis_d() +
  labs(
    title = "Distribution of Price News by Maturity",
    x = "Price News",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(plot_dir, "price_news_densities.svg"), p_density, width = 12, height = 8)

cli_h2("Box Plots by Decade")

# Add decade information
price_news_long <- price_news_long |>
  mutate(decade = floor(year(date) / 10) * 10)

p_box_decade <- ggplot(
  price_news_long |> filter(maturity %in% selected_maturities),
  aes(x = factor(decade), y = price_news, fill = maturity_label)
) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_fill_viridis_d() +
  labs(
    title = "Price News Distribution by Decade",
    x = "Decade",
    y = "Price News",
    fill = "Maturity"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(plot_dir, "price_news_boxplots_decade.svg"), p_box_decade, width = 10, height = 6)

cli_h1("3. CORRELATION VISUALIZATIONS")

cli_h2("Correlation Heatmap")

# Compute correlation matrix
cor_matrix <- cor(price_news)
colnames(cor_matrix) <- gsub("price_news_", "", colnames(cor_matrix))
rownames(cor_matrix) <- gsub("price_news_", "", rownames(cor_matrix))

# Create correlation heatmap using corrplot
svg(file.path(plot_dir, "price_news_correlation_heatmap.svg"), width = 8, height = 8)
corrplot(cor_matrix,
  method = "color",
  type = "upper",
  order = "original",
  col = colorRampPalette(c("#4575b4", "white", "#d73027"))(100),
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  title = "Price News Correlation Matrix",
  mar = c(0, 0, 2, 0)
)
dev.off()

cli_h2("Scatter Plot Matrix")

# Select subset for scatter plot matrix
if (length(price_news_vars) > 4) {
  n_pn <- length(price_news_vars)
  selected_vars <- price_news_vars[c(1, floor(n_pn / 2), n_pn)]
} else {
  selected_vars <- price_news_vars
}

# Create scatter plot matrix
pairs_data <- data_with_news[, selected_vars]
colnames(pairs_data) <- gsub("price_news_", "", selected_vars)

svg(file.path(plot_dir, "price_news_scatterplot_matrix.svg"), width = 10, height = 10)
pairs(pairs_data,
  main = "Price News Scatter Plot Matrix",
  pch = 19,
  col = rgb(0, 0, 1, 0.3),
  cex = 0.5,
  lower.panel = function(x, y, ...) {
    points(x, y, pch = 19, col = rgb(0, 0, 1, 0.3), cex = 0.5)
    abline(lm(y ~ x), col = "red", lwd = 2)
  },
  upper.panel = function(x, y, ...) {
    usr <- par("usr")
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = "complete.obs")
    txt <- format(r, digits = 2)
    text(0.5, 0.5, txt, cex = 1.5)
  }
)
dev.off()

cli_h1("4. ROLLING STATISTICS VISUALIZATIONS")

# Get rolling statistics from analysis
rolling_stats <- analysis_results$rolling_statistics

cli_h2("Rolling Mean and Volatility")

p_roll_stats <- rolling_stats |>
  filter(!is.na(roll_sd)) |>
  select(date, roll_mean, roll_sd) |>
  pivot_longer(cols = c(roll_mean, roll_sd), names_to = "statistic", values_to = "value") |>
  mutate(
    statistic_label = case_when(
      statistic == "roll_mean" ~ "Rolling Mean",
      statistic == "roll_sd" ~ "Rolling Volatility"
    )
  ) |>
  ggplot(aes(x = date, y = value, color = statistic_label)) +
  geom_line(linewidth = 1) +
  facet_wrap(~statistic_label, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Rolling Mean" = "#1f77b4", "Rolling Volatility" = "#ff7f0e")) +
  labs(
    title = "Rolling Statistics of 2-Year Price News (10-year window)",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(plot_dir, "rolling_statistics.svg"), p_roll_stats, width = 10, height = 8)

cli_h2("Rolling Volatility with Economic Events")

# Add shaded regions for notable economic periods
p_roll_vol_events <- ggplot(rolling_stats |> filter(!is.na(roll_sd)), aes(x = date, y = roll_sd)) +
  # Add recession shading (example periods - adjust as needed)
  annotate("rect",
    xmin = as.Date("1980-01-01"), xmax = as.Date("1982-12-31"),
    ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray"
  ) +
  annotate("rect",
    xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-30"),
    ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray"
  ) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.3, color = "red") +
  labs(
    title = "Rolling Volatility with Economic Recessions",
    subtitle = "Gray shaded areas indicate NBER recession periods",
    x = "Date",
    y = "Rolling Standard Deviation"
  ) +
  theme_minimal()

ggsave(
  file.path(plot_dir, "rolling_volatility_events.svg"),
  p_roll_vol_events,
  width = 10, height = 6
)

cli_h1("5. MATURITY STRUCTURE VISUALIZATIONS")

cli_h2("3D Surface Plot of Price News")

# Create grid for 3D plot
maturity_grid <- expand.grid(
  date = unique(data_with_news$date)[seq(1, length(unique(data_with_news$date)), by = 4)],
  maturity = as.numeric(gsub("price_news_", "", price_news_vars))
)

# Extract corresponding price news values
maturity_grid$price_news <- NA
for (i in seq_len(nrow(maturity_grid))) {
  date_idx <- which(data_with_news$date == maturity_grid$date[i])
  mat_var <- paste0("price_news_", maturity_grid$maturity[i])
  if (mat_var %in% names(data_with_news) && length(date_idx) > 0) {
    maturity_grid$price_news[i] <- data_with_news[[mat_var]][date_idx]
  }
}

# Create interactive 3D plot
p_3d <- plot_ly(
  data = maturity_grid,
  x = ~date,
  y = ~maturity,
  z = ~price_news,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 3,
    color = ~price_news,
    colorscale = "RdBu",
    showscale = TRUE
  )
) |>
  layout(
    title = "Price News Term Structure Evolution",
    scene = list(
      xaxis = list(title = "Date"),
      yaxis = list(title = "Maturity (years)"),
      zaxis = list(title = "Price News")
    )
  )

# Save interactive plot
htmlwidgets::saveWidget(p_3d, file.path(plot_dir, "price_news_3d_surface.html"))

cli_h2("Term Structure Snapshots")

# Select specific dates for snapshots
snapshot_dates <- c(
  "1980-03-31", "1990-03-31", "2000-03-31",
  "2010-03-31", "2020-03-31"
)
snapshot_dates <- as.Date(snapshot_dates)
snapshot_dates <- snapshot_dates[snapshot_dates %in% data_with_news$date]

if (length(snapshot_dates) > 0) {
  snapshot_data <- data_with_news |>
    filter(date %in% snapshot_dates) |>
    select(date, all_of(price_news_vars)) |>
    pivot_longer(cols = -date, names_to = "maturity", values_to = "price_news") |>
    mutate(
      maturity_num = as.numeric(gsub("price_news_", "", maturity)),
      date_label = format(date, "%Y")
    )

  p_snapshots <- ggplot(snapshot_data, aes(x = maturity_num, y = price_news, color = date_label)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_color_viridis_d() +
    labs(
      title = "Price News Term Structure at Selected Dates",
      x = "Maturity (years)",
      y = "Price News",
      color = "Year"
    ) +
    theme_minimal()

  ggsave(file.path(plot_dir, "term_structure_snapshots.svg"), p_snapshots, width = 10, height = 6)
}

cli_h1("6. RELATIONSHIP WITH ECONOMIC VARIABLES")

cli_h2("Price News vs Lagged PCs")

# Create correlation plot with lagged PCs
pcs_matrix <- as.matrix(data_with_news[, pc_lag_vars])
cor_news_pcs <- cor(price_news, pcs_matrix)

# Visualize as heatmap
svg(file.path(plot_dir, "price_news_pcs_correlation.svg"), width = 10, height = 8)
corrplot(cor_news_pcs,
  method = "color",
  col = colorRampPalette(c("#4575b4", "white", "#d73027"))(100),
  addCoef.col = "black",
  number.cex = 0.7,
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.8,
  title = "Correlation: Price News vs Lagged Principal Components",
  mar = c(0, 0, 2, 0)
)
dev.off()

cli_h2("Price News vs Consumption Growth")

# Plot relationship with consumption growth
consumption_growth <- data_with_news[[HETID_CONSTANTS$CONSUMPTION_GROWTH_COL]]

p_consumption <- data_with_news |>
  select(all_of(selected_maturities),
    consumption = all_of(HETID_CONSTANTS$CONSUMPTION_GROWTH_COL)
  ) |>
  pivot_longer(
    cols = starts_with("price_news"),
    names_to = "maturity", values_to = "price_news"
  ) |>
  mutate(maturity_label = gsub("price_news_", "", maturity)) |>
  ggplot(aes(x = consumption, y = price_news)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ paste0(maturity_label, "-year"), scales = "free") +
  labs(
    title = "Price News vs Consumption Growth",
    x = "Consumption Growth",
    y = "Price News"
  ) +
  theme_minimal()

ggsave(file.path(plot_dir, "price_news_vs_consumption.svg"), p_consumption, width = 10, height = 6)

cli_h1("7. SUMMARY VISUALIZATION DASHBOARD")

# Create a dashboard-style summary plot
p1_summary <- ggplot(
  price_news_long |> filter(maturity == price_news_vars[1]),
  aes(x = date, y = price_news)
) +
  geom_line(color = "#1f77b4", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = paste0(gsub("price_news_", "", price_news_vars[1]), "-Year Price News"),
    x = "", y = "Price News"
  ) +
  theme_minimal()

p2_summary <- ggplot(
  rolling_stats |> filter(!is.na(roll_sd)),
  aes(x = date, y = roll_sd)
) +
  geom_line(color = "#ff7f0e", linewidth = 1) +
  labs(title = "Rolling Volatility", x = "", y = "Std Dev") +
  theme_minimal()

p3_summary <- ggplot(price_news_long, aes(x = price_news)) +
  geom_histogram(bins = 50, fill = "#2ca02c", alpha = 0.7) +
  facet_wrap(~maturity_label, scales = "free") +
  labs(title = "Price News Distributions", x = "Price News", y = "Count") +
  theme_minimal()

# Combine plots
dashboard <- gridExtra::arrangeGrob(
  p1_summary, p2_summary, p3_summary,
  layout_matrix = rbind(c(1, 2), c(3, 3)),
  top = "Price News Summary Dashboard"
)

ggsave(file.path(plot_dir, "price_news_dashboard.svg"), dashboard, width = 14, height = 10)

cli_h1("Visualization Summary")

cli_alert_success("Created {.val {length(list.files(plot_dir))}} exploratory plots")
cli_ul(c(
  "Time series plots (multi-panel and overlay)",
  "Distribution plots (densities and boxplots)",
  "Correlation visualizations (heatmap and scatter matrix)",
  "Rolling statistics plots",
  "Term structure visualizations",
  "Relationship plots with economic variables",
  "Summary dashboard"
))

cli_text("All plots saved to: {.path {plot_dir}}")
