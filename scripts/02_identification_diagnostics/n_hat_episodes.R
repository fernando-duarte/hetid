# N-hat Episode Detection and Economic Context
# Ports the legacy positive n-hat analyses: detect contiguous positive n_hat_1
# episodes, map them to crisis/QE event windows, check yield-curve inversion and
# term-premium differentials, and validate the implied 1-year-ahead short-rate
# prediction against realized rates

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()

cli_h1("Analyzing Positive n-hat Episodes")

output_dir <- file.path(OUTPUT_TEMP_DIR, "identification_diagnostics")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

save_plot_pair <- function(plot, base_name) {
  ggsave(file.path(output_dir, paste0(base_name, ".png")), plot,
    width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI
  )
  ggsave(file.path(output_dir, paste0(base_name, ".svg")), plot,
    width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI
  )
}

# Monthly n-hat series (the legacy analyses ran on monthly ACM data)
acm <- extract_acm_data(data_types = c("yields", "term_premia"), frequency = "monthly")
yields <- acm[, grep("^y\\d+$", names(acm))]
term_premia <- acm[, grep("^tp\\d+$", names(acm))]
dates <- acm$date

n_hat_df <- compute_n_hat(yields, term_premia, i = 1, return_df = TRUE, dates = dates)
n_hat <- n_hat_df$n_hat

positive_idx <- which(n_hat > 0)
positive_share <- sprintf("%.1f%%", 100 * length(positive_idx) / length(n_hat))
cli_alert_info(
  "Positive n_hat_1: {length(positive_idx)} of {length(n_hat)} observations ({positive_share})"
)
if (length(positive_idx) > 0) {
  cli_h2("Dates with positive n_hat_1")
  cli_ul(format(dates[positive_idx], "%Y-%m-%d"))
}

# Crisis/QE event windows with one-sentence narratives ported from the legacy
# economic-context analysis
nhat_events <- data.frame(
  event_label = c(
    "Lehman aftermath / QE expectations", "QE2 / double-dip fears",
    "Operation Twist / S&P downgrade", "Taper tantrum aftermath"
  ),
  event_start = as.Date(c("2008-09-15", "2010-07-01", "2011-01-01", "2013-05-01")),
  event_end = as.Date(c("2009-03-31", "2010-11-30", "2011-09-30", "2013-12-31")),
  narrative = c(
    paste(
      "Lehman's collapse brought aggressive Fed rate cuts to the zero lower bound,",
      "expectations of extraordinary easing, and a flight to quality that drove",
      "Treasury yields to historic lows."
    ),
    paste(
      "With the funds rate at the zero lower bound after QE1, markets priced in QE2",
      "amid double-dip recession concerns and an emerging European debt crisis."
    ),
    paste(
      "Still at the zero lower bound, the Fed announced Operation Twist while the",
      "August 2011 S&P downgrade of US credit heightened recovery uncertainty."
    ),
    paste(
      "Markets adjusted to the Fed's discussion of tapering QE3 while still",
      "expecting short rates to stay low for an extended period."
    )
  ),
  stringsAsFactors = FALSE
)

# Contiguous positive episodes via run-length encoding, with per-episode
# yield-inversion shares and term-premium spreads
runs <- rle(n_hat > 0)
run_end <- cumsum(runs$lengths)
run_start <- run_end - runs$lengths + 1
positive_runs <- which(runs$values)

if (length(positive_runs) > 0) {
  episodes <- do.call(rbind, lapply(positive_runs, function(r) {
    obs <- seq(run_start[r], run_end[r])
    data.frame(
      start_date = dates[run_start[r]],
      end_date = dates[run_end[r]],
      n_months = runs$lengths[r],
      mean_n_hat = mean(n_hat[obs]),
      max_n_hat = max(n_hat[obs]),
      share_inverted = mean(yields$y1[obs] > yields$y2[obs]),
      mean_tp_spread = mean(term_premia$tp2[obs] - term_premia$tp1[obs])
    )
  }))
  # Label each episode with the first event window it overlaps, if any
  episodes$event_label <- vapply(seq_len(nrow(episodes)), function(k) {
    hit <- which(episodes$start_date[k] <= nhat_events$event_end &
      episodes$end_date[k] >= nhat_events$event_start)
    if (length(hit) > 0) nhat_events$event_label[hit[1]] else NA_character_
  }, character(1))
} else {
  episodes <- data.frame(
    start_date = as.Date(character(0)), end_date = as.Date(character(0)),
    n_months = integer(0), mean_n_hat = numeric(0), max_n_hat = numeric(0),
    share_inverted = numeric(0), mean_tp_spread = numeric(0),
    event_label = character(0)
  )
}

cli_h2("Contiguous positive n-hat episodes")
if (nrow(episodes) > 0) {
  print(episodes, row.names = FALSE)
} else {
  cli_alert_warning("No contiguous positive n-hat episodes detected")
}

# Term-premium differential at positive observations versus the full sample
tp_spread_overall <- mean(term_premia$tp2 - term_premia$tp1, na.rm = TRUE)
tp_spread_positive <- if (length(positive_idx) > 0) {
  mean(term_premia$tp2[positive_idx] - term_premia$tp1[positive_idx])
} else {
  NA_real_
}
cli_alert_info("Mean tp2 - tp1 when n_hat_1 > 0: {sprintf('%.4f', tp_spread_positive)} pp")
cli_alert_info("Mean tp2 - tp1 over the full sample: {sprintf('%.4f', tp_spread_overall)} pp")

# Prediction validation over the full sample: n_hat(1,t) estimates
# -E_t[y_(t+1)^(1)] in decimals, so the implied 1-year-ahead short yield in
# percent is -100 * n_hat; the realization is y1 at the first date >= t + 1 year
future_idx <- vapply(seq_along(dates), function(t) which(dates >= dates[t] + 365)[1], integer(1))
validation <- data.frame(
  date = dates, n_hat = n_hat, predicted_y1_pct = -100 * n_hat,
  realized_date = dates[future_idx], realized_y1_pct = yields$y1[future_idx]
)
validation$error_pct <- validation$predicted_y1_pct - validation$realized_y1_pct
validation <- validation[!is.na(validation$realized_y1_pct), ]

validation_summary <- list(
  n = nrow(validation),
  correlation = if (nrow(validation) > 1) {
    cor(validation$predicted_y1_pct, validation$realized_y1_pct)
  } else {
    NA_real_
  },
  mean_error = if (nrow(validation) > 0) mean(validation$error_pct) else NA_real_,
  rmse = if (nrow(validation) > 0) sqrt(mean(validation$error_pct^2)) else NA_real_
)

cli_h2("Prediction validation: implied versus realized 1-year yields")
cli_ul(c(
  paste("Observations with realizations:", validation_summary$n),
  paste("Correlation:", sprintf("%.4f", validation_summary$correlation)),
  paste("Mean error (predicted - realized, pp):", sprintf("%.4f", validation_summary$mean_error)),
  paste("RMSE (pp):", sprintf("%.4f", validation_summary$rmse))
))

validation_positive <- validation[validation$n_hat > 0, ]
if (nrow(validation_positive) > 0) {
  cli_h3("Validation at positive n-hat observations")
  print(validation_positive, row.names = FALSE)
}

# Quarterly cross-check on the stage-01 processed data; this quarterly series is
# the one underlying stage 03's c_hat
quarterly <- readRDS(DATA_RDS_PATH)
if (is.list(quarterly) && !is.data.frame(quarterly)) {
  quarterly <- as.data.frame(quarterly)
}
yields_q <- quarterly[, grep("^y\\d+$", names(quarterly))]
tp_q <- quarterly[, grep("^tp\\d+$", names(quarterly))]
n_hat_q <- compute_n_hat(yields_q, tp_q, i = 1)
positive_q <- which(n_hat_q > 0)
quarterly_check <- list(
  n_obs = length(n_hat_q), n_positive = length(positive_q),
  positive_dates = quarterly$date[positive_q]
)

cli_h2("Quarterly cross-check (stage-01 processed data)")
cli_alert_info(
  "Positive quarterly n_hat_1: {quarterly_check$n_positive} of {quarterly_check$n_obs}"
)
if (quarterly_check$n_positive > 0) {
  cli_ul(format(quarterly_check$positive_dates, "%Y-%m-%d"))
}

# Timeline of monthly n-hat with positives highlighted and event windows shaded
plot_df <- data.frame(date = dates, n_hat = n_hat, is_positive = n_hat > 0)
p_timeline <- ggplot(plot_df, aes(x = date, y = n_hat)) +
  geom_rect(
    data = nhat_events,
    aes(xmin = event_start, xmax = event_end, ymin = -Inf, ymax = Inf, fill = event_label),
    alpha = 0.15, inherit.aes = FALSE
  ) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(data = plot_df[plot_df$is_positive, ], color = "red", size = 2) +
  labs(
    title = "Expected Log Bond Price Estimator n-hat(1) Over Time",
    subtitle = "Red points mark positive values; shading marks crisis/QE event windows",
    x = "Date", y = "n-hat (decimal)", fill = "Event window"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))
save_plot_pair(p_timeline, "n_hat_timeline")

# Predicted versus realized scatter with a 45-degree reference line
annotation_label <- sprintf(
  "Correlation: %.3f\nRMSE: %.2f pp",
  validation_summary$correlation, validation_summary$rmse
)
p_scatter <- ggplot(validation, aes(x = predicted_y1_pct, y = realized_y1_pct)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(color = "steelblue", alpha = 0.6) +
  annotate("text", x = -Inf, y = Inf, label = annotation_label, hjust = -0.1, vjust = 1.3) +
  labs(
    title = "n-hat Implied 1-Year-Ahead Short Yield Versus Realized",
    subtitle = "Prediction is -100 * n-hat(1); realization is y1 about one year later",
    x = "Predicted 1-year yield (percent)", y = "Realized 1-year yield (percent)"
  ) +
  theme_minimal()
save_plot_pair(p_scatter, "n_hat_prediction_scatter")

# Persist series, episodes, validation, and summary objects
series_csv <- data.frame(
  date = dates, n_hat = n_hat, y1 = yields$y1, y2 = yields$y2,
  tp1 = term_premia$tp1, tp2 = term_premia$tp2,
  spread_y2_y1 = yields$y2 - yields$y1, is_positive = n_hat > 0
)
write.csv(series_csv, file.path(output_dir, "n_hat_series.csv"), row.names = FALSE)
write.csv(episodes, file.path(output_dir, "n_hat_episodes.csv"), row.names = FALSE)
write.csv(validation, file.path(output_dir, "n_hat_prediction_validation.csv"),
  row.names = FALSE
)

results <- list(
  n_hat_df = n_hat_df,
  episodes = episodes,
  events = nhat_events,
  validation_summary = validation_summary,
  tp_spread = list(positive_mean = tp_spread_positive, overall_mean = tp_spread_overall),
  quarterly_check = quarterly_check
)
saveRDS(results, file.path(output_dir, "n_hat_episode_results.rds"))

written_files <- file.path(output_dir, c(
  "n_hat_episode_results.rds", "n_hat_series.csv", "n_hat_episodes.csv",
  "n_hat_prediction_validation.csv", "n_hat_timeline.png", "n_hat_timeline.svg",
  "n_hat_prediction_scatter.png", "n_hat_prediction_scatter.svg"
))
cli_alert_success("n-hat episode analysis completed!")
cli_ul(written_files)
