# Heteroskedasticity Testing Utilities
# Common functions for heteroskedasticity analysis

#' Perform comprehensive heteroskedasticity tests
#'
#' Runs the requested subset of the skedastic suite. Callers whose design
#' makes a test degenerate (e.g. Anscombe when the residuals are regressed
#' on their own regressors, so fitted values are floating-point noise)
#' should exclude it via `tests`; see
#' docs/reviews/hetero-test-investigation-2026-06-10.md.
#' @param lm_model fitted linear model
#' @param var_name variable name for labeling
#' @param tests which tests to run (subset of the six defaults)
#' @param gq_deflator regressor name Goldfeld-Quandt orders by; NULL keeps
#'   skedastic's default (the data's existing row order, i.e. time here)
#' @param gq_alternative alternative hypothesis for Goldfeld-Quandt
#' @return data frame with stat/pval columns for the selected tests
perform_all_hetero_tests <- function(lm_model, var_name = "Variable",
                                     tests = c(
                                       "White", "BP", "GQ",
                                       "Harvey", "Anscombe", "CW"
                                     ),
                                     gq_deflator = NULL,
                                     gq_alternative = "greater") {
  results <- data.frame(Variable = var_name, stringsAsFactors = FALSE)

  if ("White" %in% tests) {
    white_test <- skedastic::white(lm_model)
    results$White_stat <- as.numeric(white_test$statistic)
    results$White_pval <- as.numeric(white_test$p.value)
  }

  if ("BP" %in% tests) {
    bp_test <- skedastic::breusch_pagan(lm_model)
    results$BP_stat <- as.numeric(bp_test$statistic)
    results$BP_pval <- as.numeric(bp_test$p.value)
  }

  if ("GQ" %in% tests) {
    if (!is.null(gq_deflator)) {
      regressors <- attr(terms(lm_model), "term.labels")
      if (!gq_deflator %in% regressors) {
        stop(
          "gq_deflator '", gq_deflator, "' is not a regressor of the model ",
          "(available: ", paste(regressors, collapse = ", "), ")"
        )
      }
    }
    gq_test <- skedastic::goldfeld_quandt(
      lm_model,
      deflator = if (is.null(gq_deflator)) NA else gq_deflator,
      alternative = gq_alternative
    )
    results$GQ_stat <- as.numeric(gq_test$statistic)
    results$GQ_pval <- as.numeric(gq_test$p.value)
  }

  if ("Harvey" %in% tests) {
    harvey_test <- skedastic::harvey(lm_model)
    results$Harvey_stat <- as.numeric(harvey_test$statistic)
    results$Harvey_pval <- as.numeric(harvey_test$p.value)
  }

  if ("Anscombe" %in% tests) {
    anscombe_test <- skedastic::anscombe(lm_model)
    results$Anscombe_stat <- as.numeric(anscombe_test$statistic)
    results$Anscombe_pval <- as.numeric(anscombe_test$p.value)
  }

  if ("CW" %in% tests) {
    cw_test <- skedastic::cook_weisberg(lm_model)
    results$CW_stat <- as.numeric(cw_test$statistic)
    results$CW_pval <- as.numeric(cw_test$p.value)
  }

  results
}

#' Summarize heteroskedasticity test results
#'
#' Summarizes the tests the caller ran via perform_all_hetero_tests().
#' Rates are taken over the rows where each test actually produced a
#' p-value (non-NA), so an errored suite row cannot deflate them.
#' @param hetero_results data frame with test results
#' @param significance_level significance level for tests
#' @param test_names which tests to summarize (must match the suite the
#'   caller actually ran; a missing column errors rather than silently NA)
#' @return data frame with summary
summarize_hetero_tests <- function(hetero_results, significance_level = 0.05,
                                   test_names = c(
                                     "White", "BP", "GQ",
                                     "Harvey", "Anscombe", "CW"
                                   )) {
  pval_cols <- paste0(test_names, "_pval")

  missing_cols <- setdiff(pval_cols, names(hetero_results))
  if (length(missing_cols) > 0) {
    stop(
      "summarize_hetero_tests: missing p-value column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  rejections <- unname(vapply(pval_cols, function(col) {
    sum(hetero_results[[col]] < significance_level, na.rm = TRUE)
  }, integer(1)))
  totals <- unname(vapply(pval_cols, function(col) {
    sum(!is.na(hetero_results[[col]]))
  }, integer(1)))

  data.frame(
    Test = test_names,
    Rejections = rejections,
    Total = totals,
    Percentage = ifelse(totals > 0, round(100 * rejections / totals, 1), NA_real_),
    stringsAsFactors = FALSE
  )
}
