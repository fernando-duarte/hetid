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

# Display results in narrower tables
cat("\nTime Series Properties Summary\n")
cat("==============================\n")

# Create narrower tables by splitting columns
# Basic statistics table
basic_stats <- all_ts_results[, c("Variable", "Mean", "SD", "AC1", "AC4", "N")]
basic_stats <- basic_stats[!grepl("---", basic_stats$Variable), ]

cat("\nBasic Statistics:\n")
print(kable(basic_stats, format = "simple", align = "l", digits = 4))

# Unit root and stationarity tests
unit_root_stats <- all_ts_results[, c("Variable", "ADF_stat", "ADF_pval", "KPSS_stat", "KPSS_pval", "PP_stat", "PP_pval")]
unit_root_stats <- unit_root_stats[!grepl("---", unit_root_stats$Variable), ]

cat("\nUnit Root and Stationarity Tests:\n")
print(kable(unit_root_stats, format = "simple", align = "l", digits = 4))

# Serial correlation and normality tests
serial_norm_stats <- all_ts_results[, c("Variable", "LB_stat", "LB_pval", "ARCH_stat", "ARCH_pval", "JB_stat", "JB_pval")]
serial_norm_stats <- serial_norm_stats[!grepl("---", serial_norm_stats$Variable), ]

cat("\nSerial Correlation and Normality Tests:\n")
print(kable(serial_norm_stats, format = "simple", align = "l", digits = 4))

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

# Test heteroskedasticity for all key variables
all_test_vars <- c(yield_vars, tp_vars, macro_vars)
hetero_results <- do.call(rbind, lapply(all_test_vars, function(v) {
  if (v %in% names(df)) {
    perform_hetero_tests(df[[v]], predictor_vars, v)
  }
}))

# Format results
hetero_numeric_cols <- c(
  "White_stat", "White_pval", "BP_stat", "BP_pval", "GQ_stat", "GQ_pval",
  "Harvey_stat", "Harvey_pval", "Anscombe_stat", "Anscombe_pval",
  "CW_stat", "CW_pval"
)
hetero_results[hetero_numeric_cols] <- lapply(
  hetero_results[hetero_numeric_cols],
  function(x) round(x, 4)
)

print(kable(hetero_results, format = "simple", align = "l"))

# Create comprehensive heteroskedasticity summary table
hetero_summary <- data.frame(
  Variable = hetero_results$Variable,
  White_Test = ifelse(hetero_results$White_pval < 0.05, "Reject H0", "Fail to Reject"),
  White_pval = round(hetero_results$White_pval, 4),
  BP_Test = ifelse(hetero_results$BP_pval < 0.05, "Reject H0", "Fail to Reject"),
  BP_pval = round(hetero_results$BP_pval, 4),
  GQ_Test = ifelse(hetero_results$GQ_pval < 0.05, "Reject H0", "Fail to Reject"),
  GQ_pval = round(hetero_results$GQ_pval, 4),
  Harvey_Test = ifelse(hetero_results$Harvey_pval < 0.05, "Reject H0", "Fail to Reject"),
  Harvey_pval = round(hetero_results$Harvey_pval, 4),
  Anscombe_Test = ifelse(hetero_results$Anscombe_pval < 0.05, "Reject H0", "Fail to Reject"),
  Anscombe_pval = round(hetero_results$Anscombe_pval, 4),
  CW_Test = ifelse(hetero_results$CW_pval < 0.05, "Reject H0", "Fail to Reject"),
  CW_pval = round(hetero_results$CW_pval, 4),
  stringsAsFactors = FALSE
)

# Split summary table for better console display
hetero_summary_1 <- hetero_summary[, c("Variable", "White_Test", "White_pval", "BP_Test", "BP_pval", "GQ_Test", "GQ_pval")]
hetero_summary_2 <- hetero_summary[, c("Variable", "Harvey_Test", "Harvey_pval", "Anscombe_Test", "Anscombe_pval", "CW_Test", "CW_pval")]

cat("\n\nHeteroskedasticity Test Summary - Part 1 (H0: Homoskedasticity):\n")
cat("================================================================\n")
print(kable(hetero_summary_1, format = "simple", align = "l"))

cat("\n\nHeteroskedasticity Test Summary - Part 2 (H0: Homoskedasticity):\n")
cat("================================================================\n")
print(kable(hetero_summary_2, format = "simple", align = "l"))

# Add interpretation of heteroskedasticity tests
cat("\n\nInterpretation Guide:\n")
cat("====================\n")
cat("White Test: General test for heteroskedasticity\n")
cat("Breusch-Pagan (BP): More powerful against specific forms\n")
cat("Goldfeld-Quandt (GQ): Tests for monotonic heteroskedasticity\n")
cat("Harvey Test: Tests heteroskedasticity related to fitted values\n")
cat("Anscombe Test: Tests for σ²ᵢ = σ²Xᵢᵝ form of heteroskedasticity\n")
cat("Cook-Weisberg (CW): Score test, more powerful than BP for certain alternatives\n")
cat("'Reject H0' = Evidence of heteroskedasticity (p < 0.05)\n")
cat("'Fail to Reject' = No strong evidence against homoskedasticity\n")

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
