# Time Series Properties Analysis
# Analyze autocorrelation, stationarity, and other time series characteristics;
# add heteroskedasticity tests from package skedastic

# Load required packages
library(hetid)
library(dplyr)
library(tidyr)
library(lubridate)
library(urca)
library(skedastic)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)

# Set up paths
library(here)
source(here::here("scripts/utils/common_settings.R"))

# Load consolidated data
input_path <- file.path(OUTPUT_DIR, "temp/data.rds")
data <- readRDS(input_path)

# Convert list to data frame for analysis
df <- as.data.frame(data)

# Define variable groups
yield_vars <- grep("^y\\d+$", names(df), value = TRUE)
tp_vars <- grep("^tp\\d+$", names(df), value = TRUE)
pc_lag_vars <- grep("^pc\\d+_lag1$", names(df), value = TRUE)
macro_vars <- "gr1.pcecc96"

# Create time series objects
ts_data <- ts(df[, -which(names(df) == "date")],
  start = c(year(min(df$date)), quarter(min(df$date))),
  frequency = 4
)

cat("Time Series Properties Analysis\n")
cat("===============================\n")
cat("Sample period:", format(range(df$date), "%Y-%m-%d"), "\n")
cat("Frequency: Quarterly\n")
cat("Number of observations:", nrow(df), "\n\n")

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

# Analyze all variable groups
cat("Computing time series properties for all variables...\n")

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

# Display results in smaller tables
cat("\nTime Series Properties Summary\n")
cat("==============================\n")

# Split into separate tables for better readability
yields_display <- all_ts_results[grepl("y\\d+|--- Yields", all_ts_results$Variable), ]
tp_display <- all_ts_results[grepl("tp\\d+|--- Term Premia", all_ts_results$Variable), ]
pc_display <- all_ts_results[grepl("pc\\d+_lag1|--- Lagged", all_ts_results$Variable), ]
macro_display <- all_ts_results[grepl("gr1|--- Macro", all_ts_results$Variable), ]

cat("\nYields (y1-y10):\n")
print(kable(yields_display, format = "simple", align = "l"))

cat("\nTerm Premia (tp1-tp10):\n")
print(kable(tp_display, format = "simple", align = "l"))

cat("\nLagged Principal Components:\n")
print(kable(pc_display, format = "simple", align = "l"))

cat("\nMacro Variables:\n")
print(kable(macro_display, format = "simple", align = "l"))

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

  return(data.frame(
    Variable = var_name,
    White_stat = as.numeric(white_test$statistic),
    White_pval = as.numeric(white_test$p.value),
    BP_stat = as.numeric(bp_test$statistic),
    BP_pval = as.numeric(bp_test$p.value),
    GQ_stat = as.numeric(gq_test$statistic),
    GQ_pval = as.numeric(gq_test$p.value),
    N = nrow(reg_data),
    stringsAsFactors = FALSE
  ))
}

# Prepare predictor variables (lagged PCs)
predictor_vars <- df[, pc_lag_vars]

# Test heteroskedasticity for key variables
key_vars <- c("y2", "y5", "y10", "tp2", "tp5", "tp10", "gr1.pcecc96")
hetero_results <- do.call(rbind, lapply(key_vars, function(v) {
  if (v %in% names(df)) {
    perform_hetero_tests(df[[v]], predictor_vars, v)
  }
}))

# Format results
hetero_numeric_cols <- c("White_stat", "White_pval", "BP_stat", "BP_pval", "GQ_stat", "GQ_pval")
hetero_results[hetero_numeric_cols] <- lapply(
  hetero_results[hetero_numeric_cols],
  function(x) round(x, 4)
)

print(kable(hetero_results, format = "simple", align = "l"))

# Add interpretation of heteroskedasticity tests
cat("\n\nInterpretation of Heteroskedasticity Tests:\n")
cat("==========================================\n")
cat("White Test:\n")
cat("- Null hypothesis: Homoskedasticity (constant variance)\n")
cat("- Alternative: Heteroskedasticity (non-constant variance)\n")
cat("- Reject null if p-value < 0.05 (evidence of heteroskedasticity)\n\n")

cat("Breusch-Pagan Test:\n")
cat("- Null hypothesis: Homoskedasticity\n")
cat("- Alternative: Heteroskedasticity\n")
cat("- More powerful against specific forms of heteroskedasticity\n")
cat("- Reject null if p-value < 0.05\n\n")

cat("Goldfeld-Quandt Test:\n")
cat("- Null hypothesis: Homoskedasticity\n")
cat("- Alternative: Heteroskedasticity (variance increases with fitted values)\n")
cat("- Tests for monotonic heteroskedasticity\n")
cat("- Reject null if p-value < 0.05\n\n")

# Summary of results
significant_white <- sum(hetero_results$White_pval < 0.05, na.rm = TRUE)
significant_bp <- sum(hetero_results$BP_pval < 0.05, na.rm = TRUE)
significant_gq <- sum(hetero_results$GQ_pval < 0.05, na.rm = TRUE)
total_vars <- nrow(hetero_results)

cat("Summary of Test Results:\n")
cat(
  "- White test: ", significant_white, "/", total_vars,
  " variables show significant heteroskedasticity\n"
)
cat(
  "- Breusch-Pagan test: ", significant_bp, "/", total_vars,
  " variables show significant heteroskedasticity\n"
)
cat(
  "- Goldfeld-Quandt test: ", significant_gq, "/", total_vars,
  " variables show significant heteroskedasticity\n"
)

# Save all results
output_dir <- file.path(OUTPUT_DIR, "temp/time_series_properties")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save time series properties
write.csv(all_ts_results, file.path(output_dir, "time_series_properties.csv"), row.names = FALSE)

# Save heteroskedasticity test results
write.csv(hetero_results, file.path(output_dir, "heteroskedasticity_tests.csv"), row.names = FALSE)

cat("\n\nResults saved to:", output_dir)
cat("\nFiles created:")
cat("\n- time_series_properties.csv")
cat("\n- heteroskedasticity_tests.csv")
