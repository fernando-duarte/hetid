# Heteroskedasticity Testing Utilities
# Common functions for heteroskedasticity analysis

#' Perform comprehensive heteroskedasticity tests
#' @param lm_model fitted linear model
#' @param var_name variable name for labeling
#' @return data frame with test results
perform_all_hetero_tests <- function(lm_model, var_name = "Variable") {
  results <- data.frame(Variable = var_name, stringsAsFactors = FALSE)

  # White test
  white_test <- skedastic::white(lm_model)
  results$White_stat <- as.numeric(white_test$statistic)
  results$White_pval <- as.numeric(white_test$p.value)

  # Breusch-Pagan test
  bp_test <- skedastic::breusch_pagan(lm_model)
  results$BP_stat <- as.numeric(bp_test$statistic)
  results$BP_pval <- as.numeric(bp_test$p.value)

  # Goldfeld-Quandt test
  gq_test <- skedastic::goldfeld_quandt(lm_model)
  results$GQ_stat <- as.numeric(gq_test$statistic)
  results$GQ_pval <- as.numeric(gq_test$p.value)

  # Harvey test
  harvey_test <- skedastic::harvey(lm_model)
  results$Harvey_stat <- as.numeric(harvey_test$statistic)
  results$Harvey_pval <- as.numeric(harvey_test$p.value)

  # Anscombe test
  anscombe_test <- skedastic::anscombe(lm_model)
  results$Anscombe_stat <- as.numeric(anscombe_test$statistic)
  results$Anscombe_pval <- as.numeric(anscombe_test$p.value)

  # Cook-Weisberg test
  cw_test <- skedastic::cook_weisberg(lm_model)
  results$CW_stat <- as.numeric(cw_test$statistic)
  results$CW_pval <- as.numeric(cw_test$p.value)

  results
}

#' Create heteroskedasticity diagnostic plots
#' @param lm_model fitted linear model
#' @param var_name variable name for titles
#' @param plot_dir output directory for plots
#' @param save_plots whether to save plots
#' @param display_plots whether to display plots
#' @return list of ggplot objects
create_hetero_diagnostic_plots <- function(lm_model, var_name,
                                           plot_dir = NULL,
                                           save_plots = TRUE,
                                           display_plots = TRUE) {
  # Extract values
  fitted_vals <- fitted(lm_model)
  residuals_vals <- residuals(lm_model)

  # Create data frame for plotting
  plot_data <- data.frame(
    fitted = fitted_vals,
    residuals = residuals_vals,
    sqrt_abs_resid = sqrt(abs(residuals_vals)),
    squared_resid = residuals_vals^2,
    index = seq_along(residuals_vals)
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

  # Plot 4: Residuals vs Index
  p4 <- ggplot(plot_data, aes(x = index, y = residuals)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(se = FALSE, color = "darkred", method = "loess") +
    labs(
      title = paste("Residuals vs Index:", var_name),
      x = "Observation Index", y = "Residuals"
    ) +
    theme_minimal()

  plots <- list(
    residuals_vs_fitted = p1,
    scale_location = p2,
    squared_residuals = p3,
    residuals_vs_index = p4
  )

  if (save_plots && !is.null(plot_dir)) {
    if (!dir.exists(plot_dir)) {
      dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Save individual plots
    ggsave(file.path(plot_dir, paste0(var_name, "_residuals_vs_fitted.svg")),
      p1,
      width = 8, height = 6
    )
    ggsave(file.path(plot_dir, paste0(var_name, "_scale_location.svg")),
      p2,
      width = 8, height = 6
    )
    ggsave(file.path(plot_dir, paste0(var_name, "_squared_residuals.svg")),
      p3,
      width = 8, height = 6
    )
    ggsave(file.path(plot_dir, paste0(var_name, "_residuals_vs_index.svg")),
      p4,
      width = 8, height = 6
    )

    # Create and save combined plot
    # Use arrangeGrob instead of grid.arrange for ggsave compatibility
    combined_plot <- gridExtra::arrangeGrob(p1, p2, p3, p4,
      ncol = 2,
      top = paste("Heteroskedasticity Diagnostics:", var_name)
    )

    ggsave(file.path(plot_dir, paste0(var_name, "_combined_diagnostics.svg")),
      combined_plot,
      width = 12, height = 10
    )
  }

  if (display_plots) {
    # Display combined plot in viewer
    print(gridExtra::grid.arrange(p1, p2, p3, p4,
      ncol = 2,
      top = paste("Heteroskedasticity Diagnostics:", var_name)
    ))
  }

  invisible(plots)
}

#' Summarize heteroskedasticity test results
#' @param hetero_results data frame with test results
#' @param significance_level significance level for tests
#' @return data frame with summary
summarize_hetero_tests <- function(hetero_results, significance_level = 0.05) {
  test_names <- c("White", "BP", "GQ", "Harvey", "CW")
  pval_cols <- paste0(test_names, "_pval")

  summary_df <- data.frame(
    Test = test_names,
    Rejections = NA,
    Total = nrow(hetero_results),
    Percentage = NA,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(test_names)) {
    if (pval_cols[i] %in% names(hetero_results)) {
      summary_df$Rejections[i] <- sum(hetero_results[[pval_cols[i]]] < significance_level,
        na.rm = TRUE
      )
    }
  }

  summary_df$Percentage <- round(100 * summary_df$Rejections / summary_df$Total, 1)
  summary_df
}
