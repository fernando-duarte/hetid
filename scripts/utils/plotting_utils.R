# Plotting Utilities
# Common functions for consistent plot styling and saving

#' Save plot in SVG format with consistent settings
#' @param plot ggplot object to save
#' @param filename output filename (without extension)
#' @param dir output directory
#' @param width plot width in inches
#' @param height plot height in inches
#' @return invisible TRUE
save_plot_svg <- function(plot, filename, dir = plot_dir, width = PLOT_WIDTH, height = PLOT_HEIGHT) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Check if filename already ends with .svg
  if (!grepl("\\.svg$", filename)) {
    filename <- paste0(filename, ".svg")
  }

  full_path <- file.path(dir, filename)
  ggsave(full_path, plot, width = width, height = height)
  invisible(TRUE)
}

#' Apply consistent theme to ggplot
#' @param plot ggplot object
#' @return ggplot object with applied theme
apply_theme <- function(plot) {
  plot + theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )
}

#' Create and save correlation heatmap
#' @param cor_matrix correlation matrix
#' @param title plot title
#' @param filename output filename
#' @param dir output directory
#' @return invisible TRUE
save_correlation_heatmap <- function(cor_matrix, title, filename, dir = plot_dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Check if filename already ends with .svg
  if (!grepl("\\.svg$", filename)) {
    filename <- paste0(filename, ".svg")
  }

  svglite::svglite(file.path(dir, filename), width = 8, height = 7)
  corrplot::corrplot(cor_matrix,
    method = "color", type = "upper",
    order = "original", tl.col = "black", tl.srt = 45,
    title = title, mar = c(0, 0, 2, 0)
  )
  dev.off()
  invisible(TRUE)
}

#' Create time series plot with consistent styling
#' @param data data frame with date column
#' @param x_var x variable name (usually "date")
#' @param y_vars vector of y variable names
#' @param title plot title
#' @param y_lab y-axis label
#' @param colors optional color vector
#' @return ggplot object
create_time_series_plot <- function(data, x_var = "date", y_vars, title, y_lab, colors = NULL) {
  # Reshape data to long format
  plot_data <- data %>%
    select(all_of(c(x_var, y_vars))) %>%
    pivot_longer(cols = -all_of(x_var), names_to = "variable", values_to = "value")

  p <- ggplot(plot_data, aes_string(x = x_var, y = "value", color = "variable")) +
    geom_line(linewidth = 1) +
    labs(title = title, x = "Date", y = y_lab, color = NULL)

  if (!is.null(colors)) {
    p <- p + scale_color_manual(values = colors)
  }

  apply_theme(p)
}

#' Display and save plot
#' @param plot ggplot object
#' @param filename output filename
#' @param dir output directory
#' @param width plot width
#' @param height plot height
#' @param display whether to display in RStudio viewer
#' @return invisible TRUE
display_and_save_plot <- function(plot, filename, dir = plot_dir,
                                  width = PLOT_WIDTH, height = PLOT_HEIGHT,
                                  display = TRUE) {
  if (display) {
    # Ensure no device is capturing the print output
    if (dev.cur() != 1) {
      dev.off()
    }
    print(plot)
  }
  save_plot_svg(plot, filename, dir, width, height)
  invisible(TRUE)
}
