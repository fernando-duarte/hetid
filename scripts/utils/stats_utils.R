# Statistics Utilities
# Common functions for statistical analysis

# Moving-block bootstrap index: resample a length-nn series in contiguous blocks
# of length bl (wrapping to nn rows), preserving short-run dependence. Shared by
# the paper-spec and results-companion tau* bootstraps. Seed the caller, not here.
mbb_index <- function(nn, bl) {
  bl <- min(bl, nn) # a block longer than the series collapses to one full block
  nblocks <- ceiling(nn / bl)
  starts <- sample.int(nn - bl + 1L, nblocks, replace = TRUE)
  unlist(lapply(starts, function(s) s:(s + bl - 1L)))[seq_len(nn)]
}

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
    for (i in seq_len(max_lags)) {
      stats[[paste0("AC", i)]] <- acf_values[i + 1]
    }
  }

  stats
}

#' Interpolated KPSS p-value
#'
#' Linearly interpolates the statistic over the KPSS (1992, Table 1)
#' critical values reported by ur.kpss() at the 10/5/2.5/1 percent levels
#' -- the method tseries::kpss.test uses. Clamped beyond the table: a
#' reported 0.10 means p >= 0.10 and a reported 0.01 means p <= 0.01.
#' @param stat KPSS test statistic
#' @param cval critical values from ur.kpss (10/5/2.5/1 percent)
#' @return p-value truncated to [0.01, 0.10]
kpss_pvalue <- function(stat, cval) {
  cv <- as.numeric(cval)
  p <- stats::approx(cv, c(0.10, 0.05, 0.025, 0.01), xout = stat, rule = 2)$y
  # Cross-check vs the raw critical-value bands; clamped edges are one-sided.
  tol <- 1e-12
  band_ok <-
    if (stat >= cv[4]) {
      p <= 0.01 + tol
    } else if (stat >= cv[2]) {
      p > 0.01 - tol && p <= 0.05 + tol
    } else if (stat >= cv[1]) {
      p > 0.05 - tol && p <= 0.10 + tol
    } else {
      p >= 0.10 - tol
    }
  if (!band_ok) {
    stop("kpss_pvalue: p ", p, " contradicts the critical-value band at stat ", stat)
  }
  p
}

#' Perform stationarity tests
#'
#' ADF p-values are MacKinnon (1996) response-surface values from
#' urca::punitroot() (lower tail, constant-only regression); KPSS
#' p-values come from kpss_pvalue() above; Ljung-Box p-values are exact.
#' @param x numeric vector
#' @param var_name variable name
#' @return data frame with test results
perform_stationarity_tests <- function(x, var_name) {
  results <- data.frame(Variable = var_name, stringsAsFactors = FALSE)
  n_obs <- sum(!is.na(x))

  # ADF test (drift regression; tau2 statistic)
  adf_result <- urca::ur.df(x, type = "drift", selectlags = "AIC")
  results$ADF_stat <- adf_result@teststat[1]
  results$ADF_pval <- urca::punitroot(
    adf_result@teststat[1],
    N = n_obs, trend = "c", statistic = "t"
  )

  # KPSS test (null of level stationarity)
  kpss_result <- urca::ur.kpss(x, type = "mu")
  results$KPSS_stat <- kpss_result@teststat
  results$KPSS_pval <- kpss_pvalue(kpss_result@teststat, kpss_result@cval)

  # Ljung-Box test
  lb_test <- Box.test(x, lag = 8, type = "Ljung-Box")
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
  tbl <- data |>
    gt() |>
    tab_header(
      title = title,
      subtitle = subtitle
    ) |>
    tab_options(
      table.font.size = 12,
      data_row.padding = px(2)
    )

  # Add highlighting if specified
  if (!is.null(highlight_rows)) {
    tbl <- tbl |>
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
  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]

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
    dt <- dt |>
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
    na_check <- vapply(
      data, function(x) sum(is.na(x)), integer(1)
    )
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
