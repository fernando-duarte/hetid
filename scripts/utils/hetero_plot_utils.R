# Heteroskedasticity diagnostic plots. Moved verbatim from
# hetero_test_utils.R to keep that file under the size limit; the only
# consumer is stage 01 (time_series_properties.R).

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
      x = "Fitted Values", y = "sqrt|Residuals|"
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
