# Log-scale variance-bounds figure builder: log(U_i) against maturity in months.
# Pure (defining a function with no side effect on source) so the contract test can
# drive it with fixtures without touching the manifested output.

variance_bounds_render_figure <- function(df, path) {
  stopifnot(
    is.data.frame(df), nrow(df) > 0L,
    all(c("Maturity", "Variance_Bound") %in% names(df))
  )
  fig <- ggplot2::ggplot(
    df, ggplot2::aes(x = Maturity, y = log(Variance_Bound))
  ) +
    ggplot2::geom_line(color = "blue", linewidth = 1.2) +
    ggplot2::geom_point(color = "darkblue", size = 3) +
    ggplot2::labs(
      title = "Log Variance Bounds Across Maturities",
      subtitle = "Log-scale visualization for trend analysis",
      x = "Maturity (months)",
      y = "Log(Variance Bound)"
    ) +
    ggplot2::theme_minimal()
  grDevices::svg(path, width = 10, height = 6)
  print(fig)
  grDevices::dev.off()
  invisible(path)
}
