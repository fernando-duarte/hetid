# Statistics Utilities
# Common functions for statistical analysis

#' Compute comprehensive summary statistics
#' @param x numeric vector
#' @param var_name variable name
#' @param compute_ac whether to compute autocorrelations
#' @param max_lags maximum lags for ACF
#' @return data frame with statistics
compute_summary_stats <- function(x, var_name, compute_ac = TRUE, max_lags = 2) {
  # Basic statistics
  stats <- data.frame(
    Variable = var_name,
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Skewness = moments::skewness(x, na.rm = TRUE),
    Kurtosis = moments::kurtosis(x, na.rm = TRUE),
    N = sum(!is.na(x)),
    stringsAsFactors = FALSE
  )

  # Add autocorrelations if requested
  if (compute_ac && length(x[!is.na(x)]) > max_lags) {
    acf_values <- acf(x, lag.max = max_lags, plot = FALSE, na.action = na.pass)$acf
    for (i in 1:max_lags) {
      stats[[paste0("AC", i)]] <- acf_values[i + 1]
    }
  }

  stats
}

#' Perform stationarity tests
#' @param x numeric vector
#' @param var_name variable name
#' @return data frame with test results
perform_stationarity_tests <- function(x, var_name) {
  results <- data.frame(Variable = var_name, stringsAsFactors = FALSE)

  # ADF test
  adf_test <- tryCatch(
    {
      adf_result <- urca::ur.df(x, type = "drift", selectlags = "AIC")
      list(
        statistic = adf_result@teststat[1],
        p.value = ifelse(adf_result@teststat[1] < adf_result@cval[1, 2], 0.01, 0.1)
      )
    },
    error = function(e) list(statistic = NA, p.value = NA)
  )

  results$ADF_stat <- adf_test$statistic
  results$ADF_pval <- adf_test$p.value

  # KPSS test
  kpss_test <- tryCatch(
    {
      kpss_result <- urca::ur.kpss(x, type = "mu")
      list(
        statistic = kpss_result@teststat,
        p.value = ifelse(kpss_result@teststat > kpss_result@cval[2], 0.01, 0.1)
      )
    },
    error = function(e) list(statistic = NA, p.value = NA)
  )

  results$KPSS_stat <- kpss_test$statistic
  results$KPSS_pval <- kpss_test$p.value

  # Ljung-Box test
  lb_test <- tryCatch(
    {
      Box.test(x, lag = 8, type = "Ljung-Box")
    },
    error = function(e) list(statistic = NA, p.value = NA)
  )

  results$LB_stat <- lb_test$statistic
  results$LB_pval <- lb_test$p.value

  results
}

#' Create formatted gt table with consistent styling
#' @param data data frame
#' @param title table title
#' @param subtitle optional subtitle
#' @param highlight_rows rows to highlight
#' @param highlight_color color for highlighting
#' @return gt table object
create_formatted_table <- function(data, title, subtitle = NULL,
                                   highlight_rows = NULL,
                                   highlight_color = "lightblue") {
  tbl <- data %>%
    gt() %>%
    tab_header(
      title = title,
      subtitle = subtitle
    ) %>%
    tab_options(
      table.font.size = 12,
      data_row.padding = px(2)
    )

  # Add highlighting if specified
  if (!is.null(highlight_rows)) {
    tbl <- tbl %>%
      tab_style(
        style = cell_fill(color = highlight_color),
        locations = cells_body(rows = highlight_rows)
      )
  }

  tbl
}

#' Create interactive datatable with consistent settings
#' @param data data frame
#' @param page_length number of rows per page
#' @param round_digits digits to round numeric columns
#' @return DT datatable object
create_interactive_table <- function(data, page_length = 10, round_digits = 4) {
  # Find numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  dt <- datatable(
    data,
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel")
    ),
    extensions = "Buttons",
    rownames = FALSE
  )

  # Round numeric columns
  if (length(numeric_cols) > 0) {
    dt <- dt %>%
      formatRound(columns = numeric_cols, digits = round_digits)
  }

  dt
}

#' Test data for missing values and report
#' @param data data frame or list
#' @param stop_on_na whether to stop execution if NA found
#' @return logical indicating if data is complete
check_data_completeness <- function(data, stop_on_na = TRUE) {
  if (is.data.frame(data)) {
    na_check <- colSums(is.na(data))
    has_na <- any(na_check > 0)
  } else if (is.list(data)) {
    na_check <- sapply(data, function(x) sum(is.na(x)))
    has_na <- any(na_check > 0)
  } else {
    has_na <- any(is.na(data))
  }

  if (has_na) {
    if (is.data.frame(data) || is.list(data)) {
      na_vars <- names(na_check)[na_check > 0]
      message <- paste("Missing values found in:", paste(na_vars, collapse = ", "))
    } else {
      message <- "Missing values found in data"
    }

    if (stop_on_na) {
      stop(message)
    } else {
      cli::cli_alert_warning(message)
    }
  }

  !has_na
}
