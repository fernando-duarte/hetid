# Time Series Properties Analysis
# Analyze autocorrelation, stationarity, and other time series characteristics;
# add heteroskedasticity tests from package skedastic

source(here::here("scripts/utils/common_settings.R"))
# Core packages (hetid, dplyr, tidyr, gt, DT, here, cli) loaded via common_settings.R

# Load specialized packages for this script
load_timeseries_packages() # urca, skedastic, lubridate
load_visualization_packages() # ggplot2, gridExtra, plotly
load_web_packages() # htmltools, knitr

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
  # Get stationarity test results from utility
  stat_tests <- perform_stationarity_tests(x, var_name)

  # Basic autocorrelation analysis
  acf_result <- acf(x, lag.max = max_lags, plot = FALSE, na.action = na.pass)

  # Ljung-Box test for serial correlation
  lb_test <- Box.test(x, lag = max_lags, type = "Ljung-Box")

  # Phillips-Perron test (using urca package)
  pp_result <- ur.pp(x, type = "Z-tau", model = "constant")
  pp_test <- list(
    statistic = pp_result@teststat,
    p.value = ifelse(pp_result@teststat < pp_result@cval[2], 0.01, 0.1)
  )

  # ARCH test for heteroskedasticity (using squared residuals from AR(1))
  ar_model <- ar(x, order.max = 1, method = "ols")
  residuals_sq <- ar_model$resid^2
  residuals_sq <- residuals_sq[!is.na(residuals_sq)]

  if (length(residuals_sq) > 10) {
    arch_test <- Box.test(residuals_sq, lag = 4, type = "Ljung-Box")
  } else {
    arch_test <- list(statistic = NA, p.value = NA)
  }

  # Jarque-Bera test for normality
  n <- length(x[!is.na(x)])
  if (n < 8) {
    jb_test <- list(statistic = NA, p.value = NA)
  } else {
    skew <- moments::skewness(x, na.rm = TRUE)
    kurt <- moments::kurtosis(x, na.rm = TRUE)
    jb_stat <- n * (skew^2 / 6 + (kurt - 3)^2 / 24)
    jb_pval <- 1 - pchisq(jb_stat, df = 2)
    jb_test <- list(statistic = jb_stat, p.value = jb_pval)
  }

  return(data.frame(
    Variable = var_name,
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    AC1 = acf_result$acf[2],
    AC4 = acf_result$acf[5],
    LB_stat = stat_tests$LB_stat,
    LB_pval = stat_tests$LB_pval,
    ADF_stat = stat_tests$ADF_stat,
    ADF_pval = stat_tests$ADF_pval,
    KPSS_stat = stat_tests$KPSS_stat,
    KPSS_pval = stat_tests$KPSS_pval,
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

# Helper function to create time series section headers
create_ts_section_header <- function(title) {
  data.frame(
    Variable = paste0("--- ", title, " ---"), Mean = NA, SD = NA, AC1 = NA, AC4 = NA,
    LB_stat = NA, LB_pval = NA, ADF_stat = NA, ADF_pval = NA,
    KPSS_stat = NA, KPSS_pval = NA, PP_stat = NA, PP_pval = NA,
    ARCH_stat = NA, ARCH_pval = NA, JB_stat = NA, JB_pval = NA,
    N = NA, stringsAsFactors = FALSE
  )
}

# Combine all results
all_ts_results <- rbind(
  create_ts_section_header("Yields"),
  yields_ts_results,
  create_ts_section_header("Term Premia"),
  tp_ts_results,
  create_ts_section_header("Lagged Principal Components"),
  pc_lag_ts_results,
  create_ts_section_header("Macro Variables"),
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

# Use utility function for interactive table
basic_stats_dt <- create_interactive_table(basic_stats, page_length = 15, round_digits = 4)

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

cli_h2("Unit Root and Stationarity Tests")
cli_text("These tests examine whether time series are stationary or non-stationary:")
cli_h3("ADF (Augmented Dickey-Fuller)")
cli_ul(c(
  "H0: Series has a unit root (non-stationary)",
  "H1: Series is stationary",
  "Reject H0 if p < 0.05 (evidence of stationarity)"
))
cli_h3("KPSS (Kwiatkowski-Phillips-Schmidt-Shin)")
cli_ul(c(
  "H0: Series is stationary",
  "H1: Series has a unit root (non-stationary)",
  "Reject H0 if p < 0.05 (evidence of non-stationarity)"
))
cli_h3("Phillips-Perron (PP)")
cli_ul(c(
  "H0: Series has a unit root (non-stationary)",
  "H1: Series is stationary",
  "Reject H0 if p < 0.05 (evidence of stationarity)"
))
# Clean the unit root stats table for console display
unit_root_stats_clean <- unit_root_stats
rownames(unit_root_stats_clean) <- NULL
print(kable(unit_root_stats_clean, format = "simple", align = "l", digits = 4, row.names = FALSE))

# Serial correlation and normality tests
serial_norm_stats <- all_ts_results[, c("Variable", "LB_stat", "LB_pval", "ARCH_stat", "ARCH_pval", "JB_stat", "JB_pval")]
serial_norm_stats <- serial_norm_stats[!grepl("---", serial_norm_stats$Variable), ]

cli_h2("Serial Correlation and Normality Tests")
cli_text("These tests examine serial correlation, conditional heteroskedasticity, and normality:")
cli_h3("Ljung-Box (LB)")
cli_ul(c(
  "H0: No serial correlation in the series",
  "H1: Serial correlation is present",
  "Reject H0 if p < 0.05 (evidence of serial correlation)"
))
cli_h3("ARCH")
cli_ul(c(
  "H0: No conditional heteroskedasticity (constant variance)",
  "H1: Conditional heteroskedasticity is present (time-varying variance)",
  "Reject H0 if p < 0.05 (evidence of ARCH effects)"
))
cli_h3("Jarque-Bera (JB)")
cli_ul(c(
  "H0: Series follows a normal distribution",
  "H1: Series does not follow a normal distribution",
  "Reject H0 if p < 0.05 (evidence of non-normality)"
))
# Clean the serial norm stats table for console display
serial_norm_stats_clean <- serial_norm_stats
rownames(serial_norm_stats_clean) <- NULL
print(kable(serial_norm_stats_clean, format = "simple", align = "l", digits = 4, row.names = FALSE))

# Heteroskedasticity tests using skedastic package
cli_h1("Heteroskedasticity Tests (using skedastic package)")

# Use utility function for heteroskedasticity tests
perform_hetero_tests_wrapper <- function(y, x_vars, var_name) {
  # Create data frame for regression
  reg_data <- data.frame(y = y, x_vars)
  reg_data <- reg_data[complete.cases(reg_data), ]

  if (nrow(reg_data) < 10) {
    return(data.frame(
      Variable = var_name,
      White_stat = NA, White_pval = NA,
      BP_stat = NA, BP_pval = NA,
      GQ_stat = NA, GQ_pval = NA,
      Harvey_stat = NA, Harvey_pval = NA,
      Anscombe_stat = NA, Anscombe_pval = NA,
      CW_stat = NA, CW_pval = NA,
      N = nrow(reg_data),
      stringsAsFactors = FALSE
    ))
  }

  # Fit linear model and use utility
  lm_model <- lm(y ~ ., data = reg_data)
  perform_all_hetero_tests(lm_model, var_name)
}

# Prepare predictor variables (lagged PCs)
predictor_vars <- df[, pc_lag_vars]

# Test heteroskedasticity for all variables including PCs and consumption growth
all_test_vars <- c(yield_vars, tp_vars, pc_lag_vars, macro_vars)
hetero_results <- do.call(rbind, lapply(all_test_vars, function(v) {
  if (v %in% names(df)) {
    perform_hetero_tests_wrapper(df[[v]], predictor_vars, v)
  }
}))

# Format results - check which columns are actually present
hetero_numeric_cols <- c(
  "White_stat", "White_pval", "BP_stat", "BP_pval", "GQ_stat", "GQ_pval",
  "Harvey_stat", "Harvey_pval", "CW_stat", "CW_pval"
)
# Only format columns that exist
existing_cols <- intersect(hetero_numeric_cols, names(hetero_results))
if (length(existing_cols) > 0) {
  hetero_results[existing_cols] <- lapply(
    hetero_results[existing_cols],
    function(x) round(x, 4)
  )
}

cli_h2("Heteroskedasticity Test Details")
cli_text("These tests examine whether the variance of regression residuals is constant:")
cli_text("{.strong All heteroskedasticity tests:}")
cli_ul(c(
  "H0: Homoskedasticity (constant variance of residuals)",
  "H1: Heteroskedasticity (non-constant variance of residuals)",
  "Reject H0 if p < 0.05 (evidence of heteroskedasticity)"
))
cli_text("")
cli_ul(c(
  "{.strong White Test}: General test for any form of heteroskedasticity",
  "{.strong Breusch-Pagan (BP)}: More powerful against specific forms of heteroskedasticity",
  "{.strong Goldfeld-Quandt (GQ)}: Tests for monotonic heteroskedasticity"
))

# Split heteroskedasticity results into two tables for better readability (p-values only)
# Check which columns exist
pval_cols1 <- intersect(c("Variable", "White_pval", "BP_pval", "GQ_pval"), names(hetero_results))
pval_cols2 <- intersect(c("Variable", "Harvey_pval", "CW_pval"), names(hetero_results))

hetero_table1 <- hetero_results[, pval_cols1]
hetero_table2 <- hetero_results[, pval_cols2]

cli_h3("Heteroskedasticity Tests - Part 1 (p-values)")
hetero_table1_clean <- hetero_table1
rownames(hetero_table1_clean) <- NULL
print(kable(hetero_table1_clean, format = "simple", align = "l", row.names = FALSE))

cli_text("")
cli_ul(c(
  "{.strong Harvey Test}: Tests heteroskedasticity related to fitted values",
  "{.strong Anscombe Test}: Tests for σ²ᵢ = σ²Xᵢᵝ form of heteroskedasticity",
  "{.strong Cook-Weisberg (CW)}: Score test, enhanced version of Breusch-Pagan"
))
cli_h3("Heteroskedasticity Tests - Part 2 (p-values)")
hetero_table2_clean <- hetero_table2
rownames(hetero_table2_clean) <- NULL
print(kable(hetero_table2_clean, format = "simple", align = "l", row.names = FALSE))

# Summary of heteroskedasticity test results
cli_h2("Heteroskedasticity Test Summary")

# Count rejections for each test - only count columns that exist
white_rejections <- if ("White_pval" %in% names(hetero_results)) sum(hetero_results$White_pval < 0.05, na.rm = TRUE) else 0
bp_rejections <- if ("BP_pval" %in% names(hetero_results)) sum(hetero_results$BP_pval < 0.05, na.rm = TRUE) else 0
gq_rejections <- if ("GQ_pval" %in% names(hetero_results)) sum(hetero_results$GQ_pval < 0.05, na.rm = TRUE) else 0
harvey_rejections <- if ("Harvey_pval" %in% names(hetero_results)) sum(hetero_results$Harvey_pval < 0.05, na.rm = TRUE) else 0
anscombe_rejections <- if ("Anscombe_pval" %in% names(hetero_results)) sum(hetero_results$Anscombe_pval < 0.05, na.rm = TRUE) else 0
cw_rejections <- if ("CW_pval" %in% names(hetero_results)) sum(hetero_results$CW_pval < 0.05, na.rm = TRUE) else 0
total_vars <- nrow(hetero_results)

cli_text("Variables showing heteroskedasticity (p < 0.05):")
cli_ul(c(
  paste("White Test:", white_rejections, "/", total_vars, "variables"),
  paste("Breusch-Pagan Test:", bp_rejections, "/", total_vars, "variables"),
  paste("Goldfeld-Quandt Test:", gq_rejections, "/", total_vars, "variables"),
  paste("Harvey Test:", harvey_rejections, "/", total_vars, "variables"),
  paste("Anscombe Test:", anscombe_rejections, "/", total_vars, "variables"),
  paste("Cook-Weisberg Test:", cw_rejections, "/", total_vars, "variables")
))

# Use utility function to create diagnostic plots
create_hetero_plots_wrapper <- function(var_name, y_var, predictor_vars) {
  # Create regression model
  reg_data <- data.frame(y = y_var, predictor_vars)
  reg_data <- reg_data[complete.cases(reg_data), ]
  lm_model <- lm(y ~ ., data = reg_data)

  # Use utility function for diagnostic plots
  create_hetero_diagnostic_plots(lm_model, var_name,
    plot_dir = plots_dir,
    save_plots = TRUE, display_plots = TRUE
  )
}

# Generate plots for key variables (yields, term premia, consumption growth)
key_vars <- c("y2", "y5", "y10", "tp2", "tp5", "tp10", "gr1.pcecc96")
plot_success <- sapply(key_vars, function(var) {
  if (var %in% names(df)) {
    create_hetero_plots_wrapper(var, df[[var]], predictor_vars)
  } else {
    FALSE
  }
})


# Create comprehensive test summary table
cli_h1("Comprehensive Test Summary: Reject (R) or Fail to Reject (F) H0")
cli_h2("Time Series Tests")
cli_ul(c(
  "{.strong ADF} (Augmented Dickey-Fuller): H0 = Unit root (non-stationary), H1 = Stationary",
  "{.strong KPSS} (Kwiatkowski-Phillips-Schmidt-Shin): H0 = Stationary, H1 = Unit root (non-stationary)",
  "{.strong PP} (Phillips-Perron): H0 = Unit root (non-stationary), H1 = Stationary",
  "{.strong LB} (Ljung-Box): H0 = No serial correlation, H1 = Serial correlation present",
  "{.strong ARCH}: H0 = No conditional heteroskedasticity, H1 = ARCH effects present",
  "{.strong JB} (Jarque-Bera): H0 = Normal distribution, H1 = Non-normal distribution"
))
cli_h2("Heteroskedasticity Tests")
cli_ul(c(
  "{.strong White}: H0 = Homoskedasticity, H1 = Heteroskedasticity",
  "{.strong BP} (Breusch-Pagan): H0 = Homoskedasticity, H1 = Heteroskedasticity",
  "{.strong GQ} (Goldfeld-Quandt): H0 = Homoskedasticity, H1 = Heteroskedasticity",
  "{.strong Harvey}: H0 = Homoskedasticity, H1 = Heteroskedasticity",
  "{.strong Anscombe}: H0 = Homoskedasticity, H1 = Heteroskedasticity",
  "{.strong CW} (Cook-Weisberg): H0 = Homoskedasticity, H1 = Heteroskedasticity"
))

# Create summary table with reject/fail to reject decisions
summary_table <- data.frame(
  Variable = all_ts_results$Variable[!grepl("---", all_ts_results$Variable)],
  ADF = ifelse(all_ts_results$ADF_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  KPSS = ifelse(all_ts_results$KPSS_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  PP = ifelse(all_ts_results$PP_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  LB = ifelse(all_ts_results$LB_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  ARCH = ifelse(all_ts_results$ARCH_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  JB = ifelse(all_ts_results$JB_pval[!grepl("---", all_ts_results$Variable)] < 0.05, "R", "F"),
  White = if ("White_pval" %in% names(hetero_results)) ifelse(hetero_results$White_pval < 0.05, "R", "F") else "-",
  BP = if ("BP_pval" %in% names(hetero_results)) ifelse(hetero_results$BP_pval < 0.05, "R", "F") else "-",
  GQ = if ("GQ_pval" %in% names(hetero_results)) ifelse(hetero_results$GQ_pval < 0.05, "R", "F") else "-",
  Harvey = if ("Harvey_pval" %in% names(hetero_results)) ifelse(hetero_results$Harvey_pval < 0.05, "R", "F") else "-",
  Anscombe = if ("Anscombe_pval" %in% names(hetero_results)) ifelse(hetero_results$Anscombe_pval < 0.05, "R", "F") else "-",
  CW = if ("CW_pval" %in% names(hetero_results)) ifelse(hetero_results$CW_pval < 0.05, "R", "F") else "-",
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
