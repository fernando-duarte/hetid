# Time Series Properties Analysis
# Analyze autocorrelation, stationarity, and other time series characteristics;
# add heteroskedasticity tests from package skedastic

library(hetid)
library(dplyr)
library(tidyr)
library(lubridate)
library(urca)
library(skedastic)
library(ggplot2)
library(gridExtra)
library(gt)
library(DT)
library(htmltools)
library(plotly)
library(here)
source(here::here("scripts/utils/common_settings.R"))

input_path <- file.path(OUTPUT_DIR, "temp/data.rds")
data <- readRDS(input_path)
df <- as.data.frame(data)

yield_vars <- grep("^y\\d+$", names(df), value = TRUE)
tp_vars <- grep("^tp\\d+$", names(df), value = TRUE)
pc_lag_vars <- grep("^pc\\d+_lag1$", names(df), value = TRUE)
macro_vars <- "gr1.pcecc96"

ts_data <- ts(df[, -which(names(df) == "date")],
  start = c(year(min(df$date)), quarter(min(df$date))),
  frequency = 4
)

html_output_dir <- file.path(OUTPUT_DIR, "temp/time_series_properties/html")
dir.create(html_output_dir, recursive = TRUE, showWarnings = FALSE)

plots_dir <- file.path(OUTPUT_DIR, "temp/time_series_properties/plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

cli_h1("Time Series Properties Analysis")

# Function to perform comprehensive time series tests
analyze_time_series <- function(x, var_name, max_lags = 8) {
  # Basic autocorrelation analysis
  acf_result <- acf(x, lag.max = max_lags, plot = FALSE, na.action = na.pass)

  # Ljung-Box test for serial correlation
  lb_test <- Box.test(x, lag = max_lags, type = "Ljung-Box")

  # Augmented Dickey-Fuller test for unit root (using urca package)
  adf_test <- tryCatch(
    {
      adf_result <- ur.df(x, type = "drift", selectlags = "AIC")
      list(
        statistic = adf_result@teststat[1],
        p.value = ifelse(adf_result@teststat[1] < adf_result@cval[1, 2], 0.01, 0.1)
      )
    },
    error = function(e) {
      list(statistic = NA, p.value = NA, method = "ADF test failed")
    }
  )

  # KPSS test for stationarity (using urca package)
  kpss_test <- tryCatch(
    {
      kpss_result <- ur.kpss(x, type = "mu")
      list(
        statistic = kpss_result@teststat,
        p.value = ifelse(kpss_result@teststat > kpss_result@cval[2], 0.01, 0.1)
      )
    },
    error = function(e) {
      list(statistic = NA, p.value = NA, method = "KPSS test failed")
    }
  )

  # Phillips-Perron test (using urca package)
  pp_test <- tryCatch(
    {
      pp_result <- ur.pp(x, type = "Z-tau", model = "constant")
      list(
        statistic = pp_result@teststat,
        p.value = ifelse(pp_result@teststat < pp_result@cval[2], 0.01, 0.1)
      )
    },
    error = function(e) {
      list(statistic = NA, p.value = NA, method = "PP test failed")
    }
  )

  # ARCH test for heteroskedasticity (using squared residuals from AR(1))
  arch_test <- tryCatch(
    {
      ar_model <- ar(x, order.max = 1, method = "ols")
      residuals_sq <- ar_model$resid^2
      residuals_sq <- residuals_sq[!is.na(residuals_sq)]
      if (length(residuals_sq) > 10) {
        Box.test(residuals_sq, lag = 4, type = "Ljung-Box")
      } else {
        list(statistic = NA, p.value = NA, method = "ARCH test - insufficient data")
      }
    },
    error = function(e) {
      list(statistic = NA, p.value = NA, method = "ARCH test failed")
    }
  )

  # Jarque-Bera test for normality (manual implementation)
  jb_test <- tryCatch(
    {
      n <- length(x[!is.na(x)])
      if (n < 8) {
        list(statistic = NA, p.value = NA)
      } else {
        skew <- moments::skewness(x, na.rm = TRUE)
        kurt <- moments::kurtosis(x, na.rm = TRUE)
        jb_stat <- n * (skew^2 / 6 + (kurt - 3)^2 / 24)
        jb_pval <- 1 - pchisq(jb_stat, df = 2)
        list(statistic = jb_stat, p.value = jb_pval)
      }
    },
    error = function(e) {
      list(statistic = NA, p.value = NA, method = "JB test failed")
    }
  )

  return(data.frame(
    Variable = var_name,
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    AC1 = acf_result$acf[2],
    AC4 = acf_result$acf[5],
    LB_stat = lb_test$statistic,
    LB_pval = lb_test$p.value,
    ADF_stat = adf_test$statistic,
    ADF_pval = adf_test$p.value,
    KPSS_stat = kpss_test$statistic,
    KPSS_pval = kpss_test$p.value,
    PP_stat = pp_test$statistic,
    PP_pval = pp_test$p.value,
    ARCH_stat = arch_test$statistic,
    ARCH_pval = arch_test$p.value,
    JB_stat = jb_test$statistic,
    JB_pval = jb_test$p.value,
    N = sum(!is.na(x)),
    stringsAsFactors = FALSE
  ))
}

# Yields
yields_ts_results <- do.call(rbind, lapply(yield_vars, function(v) {
  analyze_time_series(df[[v]], v)
}))

# Term premia
tp_ts_results <- do.call(rbind, lapply(tp_vars, function(v) {
  analyze_time_series(df[[v]], v)
}))

# Lagged principal components
pc_lag_ts_results <- do.call(rbind, lapply(pc_lag_vars, function(v) {
  analyze_time_series(df[[v]], v)
}))

# Macro variables
macro_ts_results <- analyze_time_series(df[[macro_vars]], macro_vars)

# Combine all results
all_ts_results <- rbind(
  data.frame(
    Variable = "--- Yields ---", Mean = NA, SD = NA, AC1 = NA, AC4 = NA,
    LB_stat = NA, LB_pval = NA, ADF_stat = NA, ADF_pval = NA,
    KPSS_stat = NA, KPSS_pval = NA, PP_stat = NA, PP_pval = NA,
    ARCH_stat = NA, ARCH_pval = NA, JB_stat = NA, JB_pval = NA,
    N = NA, stringsAsFactors = FALSE
  ),
  yields_ts_results,
  data.frame(
    Variable = "--- Term Premia ---", Mean = NA, SD = NA, AC1 = NA, AC4 = NA,
    LB_stat = NA, LB_pval = NA, ADF_stat = NA, ADF_pval = NA,
    KPSS_stat = NA, KPSS_pval = NA, PP_stat = NA, PP_pval = NA,
    ARCH_stat = NA, ARCH_pval = NA, JB_stat = NA, JB_pval = NA,
    N = NA, stringsAsFactors = FALSE
  ),
  tp_ts_results,
  data.frame(
    Variable = "--- Lagged Principal Components ---", Mean = NA, SD = NA,
    AC1 = NA, AC4 = NA, LB_stat = NA, LB_pval = NA, ADF_stat = NA,
    ADF_pval = NA, KPSS_stat = NA, KPSS_pval = NA, PP_stat = NA,
    PP_pval = NA, ARCH_stat = NA, ARCH_pval = NA, JB_stat = NA,
    JB_pval = NA, N = NA, stringsAsFactors = FALSE
  ),
  pc_lag_ts_results,
  data.frame(
    Variable = "--- Macro Variables ---", Mean = NA, SD = NA, AC1 = NA, AC4 = NA,
    LB_stat = NA, LB_pval = NA, ADF_stat = NA, ADF_pval = NA,
    KPSS_stat = NA, KPSS_pval = NA, PP_stat = NA, PP_pval = NA,
    ARCH_stat = NA, ARCH_pval = NA, JB_stat = NA, JB_pval = NA,
    N = NA, stringsAsFactors = FALSE
  ),
  macro_ts_results
)

# Format numeric columns
numeric_cols <- c(
  "Mean", "SD", "AC1", "AC4", "LB_stat", "LB_pval", "ADF_stat", "ADF_pval",
  "KPSS_stat", "KPSS_pval", "PP_stat", "PP_pval", "ARCH_stat", "ARCH_pval",
  "JB_stat", "JB_pval"
)
all_ts_results[numeric_cols] <- lapply(all_ts_results[numeric_cols], function(x) round(x, 4))

# Display results in narrower tables
cli_h1("Time Series Properties Summary")

# Create narrower tables by splitting columns
# Basic statistics table
basic_stats <- all_ts_results[, c("Variable", "Mean", "SD", "AC1", "AC4", "N")]
basic_stats <- basic_stats[!grepl("---", basic_stats$Variable), ]

cli_h2("Basic Statistics")

# Create interactive HTML table for basic statistics
basic_stats_dt <- datatable(
  basic_stats,
  caption = htmltools::tags$caption(
    style = "caption-side: top; text-align: left; font-size: 16px; font-weight: bold;",
    "Basic Statistics",
    htmltools::br(),
    htmltools::tags$small(
      "Mean: Average value over sample period | ",
      "SD: Standard deviation (volatility) | ",
      "AC1: First-order autocorrelation (persistence) | ",
      "AC4: Fourth-order autocorrelation (seasonal patterns)"
    )
  ),
  options = list(
    pageLength = 15,
    scrollX = TRUE,
    dom = "Bfrtip",
    buttons = c("copy", "csv", "excel")
  ),
  extensions = "Buttons"
) %>%
  formatRound(columns = c("Mean", "SD", "AC1", "AC4"), digits = 4)

# Save HTML table
htmlwidgets::saveWidget(
  basic_stats_dt,
  file = file.path(html_output_dir, "basic_statistics.html"),
  selfcontained = TRUE
)

basic_stats_clean <- basic_stats
rownames(basic_stats_clean) <- NULL
print(kable(basic_stats_clean, format = "simple", align = "l", digits = 4, row.names = FALSE))

# Unit root and stationarity tests
unit_root_stats <- all_ts_results[, c("Variable", "ADF_stat", "ADF_pval", "KPSS_stat", "KPSS_pval", "PP_stat", "PP_pval")]
unit_root_stats <- unit_root_stats[!grepl("---", unit_root_stats$Variable), ]

cat("\nUnit Root and Stationarity Tests:\n")
cat("=================================\n")
cat("These tests examine whether time series are stationary or non-stationary:\n\n")
cat("ADF (Augmented Dickey-Fuller):\n")
cat("- H0: Series has a unit root (non-stationary)\n")
cat("- H1: Series is stationary\n")
cat("- Reject H0 if p < 0.05 (evidence of stationarity)\n\n")
cat("KPSS (Kwiatkowski-Phillips-Schmidt-Shin):\n")
cat("- H0: Series is stationary\n")
cat("- H1: Series has a unit root (non-stationary)\n")
cat("- Reject H0 if p < 0.05 (evidence of non-stationarity)\n\n")
cat("Phillips-Perron (PP):\n")
cat("- H0: Series has a unit root (non-stationary)\n")
cat("- H1: Series is stationary\n")
cat("- Reject H0 if p < 0.05 (evidence of stationarity)\n\n")
# Clean the unit root stats table for console display
unit_root_stats_clean <- unit_root_stats
rownames(unit_root_stats_clean) <- NULL
print(kable(unit_root_stats_clean, format = "simple", align = "l", digits = 4, row.names = FALSE))

# Serial correlation and normality tests
serial_norm_stats <- all_ts_results[, c("Variable", "LB_stat", "LB_pval", "ARCH_stat", "ARCH_pval", "JB_stat", "JB_pval")]
serial_norm_stats <- serial_norm_stats[!grepl("---", serial_norm_stats$Variable), ]

cat("\nSerial Correlation and Normality Tests:\n")
cat("======================================\n")
cat("These tests examine serial correlation, conditional heteroskedasticity, and normality:\n\n")
cat("Ljung-Box (LB):\n")
cat("- H0: No serial correlation in the series\n")
cat("- H1: Serial correlation is present\n")
cat("- Reject H0 if p < 0.05 (evidence of serial correlation)\n\n")
cat("ARCH:\n")
cat("- H0: No conditional heteroskedasticity (constant variance)\n")
cat("- H1: Conditional heteroskedasticity is present (time-varying variance)\n")
cat("- Reject H0 if p < 0.05 (evidence of ARCH effects)\n\n")
cat("Jarque-Bera (JB):\n")
cat("- H0: Series follows a normal distribution\n")
cat("- H1: Series does not follow a normal distribution\n")
cat("- Reject H0 if p < 0.05 (evidence of non-normality)\n\n")
# Clean the serial norm stats table for console display
serial_norm_stats_clean <- serial_norm_stats
rownames(serial_norm_stats_clean) <- NULL
print(kable(serial_norm_stats_clean, format = "simple", align = "l", digits = 4, row.names = FALSE))

# Heteroskedasticity tests using skedastic package
cat("\n\nHeteroskedasticity Tests (using skedastic package)\n")
cat("==================================================\n")

# Function to perform heteroskedasticity tests
perform_hetero_tests <- function(y, x_vars, var_name) {
  # Create data frame for regression
  reg_data <- data.frame(y = y, x_vars)
  reg_data <- reg_data[complete.cases(reg_data), ]

  if (nrow(reg_data) < 10) {
    return(data.frame(
      Variable = var_name,
      White_stat = NA, White_pval = NA,
      BP_stat = NA, BP_pval = NA,
      GQ_stat = NA, GQ_pval = NA,
      N = nrow(reg_data),
      stringsAsFactors = FALSE
    ))
  }

  # Fit linear model
  lm_model <- lm(y ~ ., data = reg_data)

  # White test
  white_test <- tryCatch(
    {
      white(lm_model)
    },
    error = function(e) {
      list(statistic = NA, p.value = NA)
    }
  )

  # Breusch-Pagan test
  bp_test <- tryCatch(
    {
      breusch_pagan(lm_model)
    },
    error = function(e) {
      list(statistic = NA, p.value = NA)
    }
  )

  # Goldfeld-Quandt test
  gq_test <- tryCatch(
    {
      goldfeld_quandt(lm_model)
    },
    error = function(e) {
      list(statistic = NA, p.value = NA)
    }
  )

  # Harvey test
  harvey_test <- tryCatch(
    {
      harvey(lm_model)
    },
    error = function(e) {
      list(statistic = NA, p.value = NA)
    }
  )

  # Anscombe test
  anscombe_test <- tryCatch(
    {
      anscombe(lm_model)
    },
    error = function(e) {
      list(statistic = NA, p.value = NA)
    }
  )



  # Cook-Weisberg test
  cw_test <- tryCatch(
    {
      cook_weisberg(lm_model)
    },
    error = function(e) {
      list(statistic = NA, p.value = NA)
    }
  )

  return(data.frame(
    Variable = var_name,
    White_stat = as.numeric(white_test$statistic),
    White_pval = as.numeric(white_test$p.value),
    BP_stat = as.numeric(bp_test$statistic),
    BP_pval = as.numeric(bp_test$p.value),
    GQ_stat = as.numeric(gq_test$statistic),
    GQ_pval = as.numeric(gq_test$p.value),
    Harvey_stat = as.numeric(harvey_test$statistic),
    Harvey_pval = as.numeric(harvey_test$p.value),
    Anscombe_stat = as.numeric(anscombe_test$statistic),
    Anscombe_pval = as.numeric(anscombe_test$p.value),
    CW_stat = as.numeric(cw_test$statistic),
    CW_pval = as.numeric(cw_test$p.value),
    N = nrow(reg_data),
    stringsAsFactors = FALSE
  ))
}

# Prepare predictor variables (lagged PCs)
predictor_vars <- df[, pc_lag_vars]

# Test heteroskedasticity for all variables including PCs and consumption growth
all_test_vars <- c(yield_vars, tp_vars, pc_lag_vars, macro_vars)
hetero_results <- do.call(rbind, lapply(all_test_vars, function(v) {
  if (v %in% names(df)) {
    perform_hetero_tests(df[[v]], predictor_vars, v)
  }
}))

# Format results
hetero_numeric_cols <- c(
  "White_stat", "White_pval", "BP_stat", "BP_pval", "GQ_stat", "GQ_pval",
  "Harvey_stat", "Harvey_pval", "Anscombe_stat", "Anscombe_pval", "CW_stat", "CW_pval"
)
hetero_results[hetero_numeric_cols] <- lapply(
  hetero_results[hetero_numeric_cols],
  function(x) round(x, 4)
)

cat("\n\nHeteroskedasticity Tests (using skedastic package)\n")
cat("==================================================\n")
cat("These tests examine whether the variance of regression residuals is constant:\n\n")
cat("All heteroskedasticity tests:\n")
cat("- H0: Homoskedasticity (constant variance of residuals)\n")
cat("- H1: Heteroskedasticity (non-constant variance of residuals)\n")
cat("- Reject H0 if p < 0.05 (evidence of heteroskedasticity)\n\n")

cat("White Test: General test for any form of heteroskedasticity\n")
cat("Breusch-Pagan (BP): More powerful against specific forms of heteroskedasticity\n")
cat("Goldfeld-Quandt (GQ): Tests for monotonic heteroskedasticity\n\n")

# Split heteroskedasticity results into two tables for better readability (p-values only)
hetero_table1 <- hetero_results[, c("Variable", "White_pval", "BP_pval", "GQ_pval")]
hetero_table2 <- hetero_results[, c("Variable", "Harvey_pval", "Anscombe_pval", "CW_pval")]

cat("Heteroskedasticity Tests - Part 1 (p-values):\n")
hetero_table1_clean <- hetero_table1
rownames(hetero_table1_clean) <- NULL
print(kable(hetero_table1_clean, format = "simple", align = "l", row.names = FALSE))

cat("\nHarvey Test: Tests heteroskedasticity related to fitted values\n")
cat("Anscombe Test: Tests for σ²ᵢ = σ²Xᵢᵝ form of heteroskedasticity\n")
cat("Cook-Weisberg (CW): Score test, enhanced version of Breusch-Pagan\n\n")

cat("Heteroskedasticity Tests - Part 2 (p-values):\n")
hetero_table2_clean <- hetero_table2
rownames(hetero_table2_clean) <- NULL
print(kable(hetero_table2_clean, format = "simple", align = "l", row.names = FALSE))

# Summary of heteroskedasticity test results
cli_h2("Heteroskedasticity Test Summary")

# Count rejections for each test
white_rejections <- sum(hetero_results$White_pval < 0.05, na.rm = TRUE)
bp_rejections <- sum(hetero_results$BP_pval < 0.05, na.rm = TRUE)
gq_rejections <- sum(hetero_results$GQ_pval < 0.05, na.rm = TRUE)
harvey_rejections <- sum(hetero_results$Harvey_pval < 0.05, na.rm = TRUE)
anscombe_rejections <- sum(hetero_results$Anscombe_pval < 0.05, na.rm = TRUE)
cw_rejections <- sum(hetero_results$CW_pval < 0.05, na.rm = TRUE)
total_vars <- nrow(hetero_results)

cli_text("Variables showing heteroskedasticity (p < 0.05):")
cli_ul(c(
  paste("White Test:", cli_col_red(white_rejections), "/", total_vars, "variables"),
  paste("Breusch-Pagan Test:", cli_col_red(bp_rejections), "/", total_vars, "variables"),
  paste("Goldfeld-Quandt Test:", cli_col_red(gq_rejections), "/", total_vars, "variables"),
  paste("Harvey Test:", cli_col_red(harvey_rejections), "/", total_vars, "variables"),
  paste("Anscombe Test:", cli_col_red(anscombe_rejections), "/", total_vars, "variables"),
  paste("Cook-Weisberg Test:", cli_col_red(cw_rejections), "/", total_vars, "variables")
))

# Function to create diagnostic plots for a variable
create_hetero_plots <- function(var_name, y_var, predictor_vars) {
  tryCatch(
    {
      # Create regression model
      reg_data <- data.frame(y = y_var, predictor_vars)
      reg_data <- reg_data[complete.cases(reg_data), ]
      lm_model <- lm(y ~ ., data = reg_data)

      # Create manual diagnostic plots using ggplot2 (more reliable)
      fitted_vals <- fitted(lm_model)
      residuals_vals <- residuals(lm_model)

      # Create data frame for plotting
      plot_data <- data.frame(
        fitted = fitted_vals,
        residuals = residuals_vals,
        sqrt_abs_resid = sqrt(abs(residuals_vals)),
        squared_resid = residuals_vals^2,
        index = 1:length(residuals_vals)
      )

      # Plot 1: Residuals vs Fitted
      p1 <- ggplot(plot_data, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6, color = "blue") +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        geom_smooth(se = FALSE, color = "darkred", method = "loess") +
        labs(
          title = paste("Residuals vs Fitted:", var_name),
          x = "Fitted Values", y = "Residuals"
        ) +
        theme_minimal()

      # Plot 2: Scale-Location Plot
      p2 <- ggplot(plot_data, aes(x = fitted, y = sqrt_abs_resid)) +
        geom_point(alpha = 0.6, color = "blue") +
        geom_smooth(se = FALSE, color = "darkred", method = "loess") +
        labs(
          title = paste("Scale-Location Plot:", var_name),
          x = "Fitted Values", y = "√|Residuals|"
        ) +
        theme_minimal()

      # Plot 3: Squared Residuals vs Fitted
      p3 <- ggplot(plot_data, aes(x = fitted, y = squared_resid)) +
        geom_point(alpha = 0.6, color = "blue") +
        geom_smooth(se = FALSE, color = "darkred", method = "loess") +
        labs(
          title = paste("Squared Residuals vs Fitted:", var_name),
          x = "Fitted Values", y = "Squared Residuals"
        ) +
        theme_minimal()

      # Plot 4: Residuals vs Index (time series pattern)
      p4 <- ggplot(plot_data, aes(x = index, y = residuals)) +
        geom_point(alpha = 0.6, color = "blue") +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        geom_smooth(se = FALSE, color = "darkred", method = "loess") +
        labs(
          title = paste("Residuals vs Index:", var_name),
          x = "Observation Index", y = "Residuals"
        ) +
        theme_minimal()

      # Combine plots into a 2x2 grid for viewer display
      combined_plot <- gridExtra::grid.arrange(p1, p2, p3, p4,
        ncol = 2,
        top = paste("Heteroskedasticity Diagnostics:", var_name)
      )

      # Display in RStudio viewer
      print(combined_plot)

      # Save individual plots
      ggsave(file.path(plots_dir, paste0(var_name, "_residuals_vs_fitted.png")), p1, width = 8, height = 6)
      ggsave(file.path(plots_dir, paste0(var_name, "_scale_location.png")), p2, width = 8, height = 6)
      ggsave(file.path(plots_dir, paste0(var_name, "_squared_residuals.png")), p3, width = 8, height = 6)
      ggsave(file.path(plots_dir, paste0(var_name, "_residuals_vs_index.png")), p4, width = 8, height = 6)

      # Also save the combined plot
      ggsave(file.path(plots_dir, paste0(var_name, "_combined_diagnostics.png")), combined_plot, width = 12, height = 10)

      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

# Generate plots for key variables (yields, term premia, consumption growth)
key_vars <- c("y2", "y5", "y10", "tp2", "tp5", "tp10", "gr1.pcecc96")
plot_success <- sapply(key_vars, function(var) {
  if (var %in% names(df)) {
    create_hetero_plots(var, df[[var]], predictor_vars)
  } else {
    FALSE
  }
})


# Create comprehensive test summary table
cat("\n\nComprehensive Test Summary: Reject (R) or Fail to Reject (F) H0\n")
cat("===============================================================\n")
cat("Time Series Tests:\n")
cat("- ADF (Augmented Dickey-Fuller): H0 = Unit root (non-stationary), H1 = Stationary\n")
cat("- KPSS (Kwiatkowski-Phillips-Schmidt-Shin): H0 = Stationary, H1 = Unit root (non-stationary)\n")
cat("- PP (Phillips-Perron): H0 = Unit root (non-stationary), H1 = Stationary\n")
cat("- LB (Ljung-Box): H0 = No serial correlation, H1 = Serial correlation present\n")
cat("- ARCH: H0 = No conditional heteroskedasticity, H1 = ARCH effects present\n")
cat("- JB (Jarque-Bera): H0 = Normal distribution, H1 = Non-normal distribution\n\n")
cat("Heteroskedasticity Tests:\n")
cat("- White: H0 = Homoskedasticity, H1 = Heteroskedasticity\n")
cat("- BP (Breusch-Pagan): H0 = Homoskedasticity, H1 = Heteroskedasticity\n")
cat("- GQ (Goldfeld-Quandt): H0 = Homoskedasticity, H1 = Heteroskedasticity\n")
cat("- Harvey: H0 = Homoskedasticity, H1 = Heteroskedasticity\n")
cat("- Anscombe: H0 = Homoskedasticity, H1 = Heteroskedasticity\n")
cat("- CW (Cook-Weisberg): H0 = Homoskedasticity, H1 = Heteroskedasticity\n\n")

# Create summary table with reject/fail to reject decisions
summary_table <- data.frame(
  Variable = all_ts_results$Variable[!grepl("---", all_ts_results$Variable)],
  ADF = ifelse(all_ts_results$ADF_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  KPSS = ifelse(all_ts_results$KPSS_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  PP = ifelse(all_ts_results$PP_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  LB = ifelse(all_ts_results$LB_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  ARCH = ifelse(all_ts_results$ARCH_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  JB = ifelse(all_ts_results$JB_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  White = ifelse(hetero_results$White_pval < 0.05, "R", "F"),
  BP = ifelse(hetero_results$BP_pval < 0.05, "R", "F"),
  GQ = ifelse(hetero_results$GQ_pval < 0.05, "R", "F"),
  Harvey = ifelse(hetero_results$Harvey_pval < 0.05, "R", "F"),
  Anscombe = ifelse(hetero_results$Anscombe_pval < 0.05, "R", "F"),
  CW = ifelse(hetero_results$CW_pval < 0.05, "R", "F"),
  stringsAsFactors = FALSE
)

# Replace NA with "-" for cleaner display
summary_table[is.na(summary_table)] <- "-"

# Create interactive HTML table for comprehensive summary
summary_dt <- datatable(
  summary_table,
  caption = htmltools::tags$caption(
    style = "caption-side: top; text-align: left; font-size: 16px; font-weight: bold;",
    "Comprehensive Test Summary: Reject (R) or Fail to Reject (F) H0",
    htmltools::br(),
    htmltools::tags$small(
      htmltools::strong("Time Series Tests:"), htmltools::br(),
      "• ADF (Augmented Dickey-Fuller): H0 = Unit root (non-stationary)", htmltools::br(),
      "• KPSS (Kwiatkowski-Phillips-Schmidt-Shin): H0 = Stationary", htmltools::br(),
      "• PP (Phillips-Perron): H0 = Unit root (non-stationary)", htmltools::br(),
      "• LB (Ljung-Box): H0 = No serial correlation", htmltools::br(),
      "• ARCH: H0 = No conditional heteroskedasticity", htmltools::br(),
      "• JB (Jarque-Bera): H0 = Normal distribution", htmltools::br(),
      htmltools::br(),
      htmltools::strong("Heteroskedasticity Tests (All H0 = Homoskedasticity):"), htmltools::br(),
      "• White: General test for any form of heteroskedasticity", htmltools::br(),
      "• BP (Breusch-Pagan): More powerful against specific forms", htmltools::br(),
      "• GQ (Goldfeld-Quandt): Tests for monotonic heteroskedasticity", htmltools::br(),
      "• Harvey: Tests heteroskedasticity related to fitted values", htmltools::br(),
      "• Anscombe: Tests for σ²ᵢ = σ²Xᵢᵝ form", htmltools::br(),
      "• CW (Cook-Weisberg): Score test, enhanced version of BP"
    )
  ),
  options = list(
    pageLength = 30,
    scrollX = TRUE,
    dom = "Bfrtip",
    buttons = c("copy", "csv", "excel"),
    columnDefs = list(
      list(className = "dt-center", targets = 1:12)
    )
  ),
  extensions = "Buttons"
) %>%
  formatStyle(
    columns = c("ADF", "PP", "LB", "ARCH", "JB", "White", "BP", "GQ", "Harvey", "Anscombe", "CW"),
    backgroundColor = styleEqual("R", "#ffcccc"),
    color = styleEqual("R", "#cc0000")
  ) %>%
  formatStyle(
    columns = "KPSS",
    backgroundColor = styleEqual("R", "#ffcccc"),
    color = styleEqual("R", "#cc0000")
  )

# Save HTML table
htmlwidgets::saveWidget(
  summary_dt,
  file = file.path(html_output_dir, "comprehensive_test_summary.html"),
  selfcontained = TRUE
)

summary_table_clean <- summary_table
rownames(summary_table_clean) <- NULL
print(kable(summary_table_clean, format = "simple", align = "l", row.names = FALSE))

output_dir <- file.path(OUTPUT_DIR, "temp/time_series_properties")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(all_ts_results, file.path(output_dir, "time_series_properties.csv"), row.names = FALSE)
write.csv(hetero_results, file.path(output_dir, "heteroskedasticity_tests.csv"), row.names = FALSE)
