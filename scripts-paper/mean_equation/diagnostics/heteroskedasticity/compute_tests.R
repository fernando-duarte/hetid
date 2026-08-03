# Compute the heteroskedasticity battery and Lewbel relevance diagnostics, once
# conditioning on Y2 (the SDF-news PCs) and once on W2 (the reduced-form
# residuals). Identification draws its curvature from W2, so that panel speaks
# to Lewbel relevance directly; the Y2 panel is retained because it is what the
# published table has always reported. The two coincide only under the
# beta2R = 0 null, which the published mean specification does not impose.

paper_source_once(paper_path("support", "diagnostics", "heteroskedasticity_tests.R"))
paper_source_once(paper_path("support", "diagnostics", "identification_diagnostics.R"))
paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path(
  "mean_equation", "diagnostics", "heteroskedasticity", "battery.R"
))

w1 <- set_id_mean_eq$w1
y1 <- set_id_mean_eq$y1
y2 <- set_id_mean_eq$y2
w2 <- set_id_mean_eq$w2
z <- set_id_mean_eq$z
z_mat <- matrix(z, ncol = 1, dimnames = list(NULL, "z"))

hetero_fmt <- function(x, d = PAPER_REPORTING_CONTROL$precision$diagnostic_table) {
  formatC(x, format = "f", digits = d)
}

pcell <- function(x) {
  if (!is.finite(x)) {
    return(PAPER_NA_TOKEN)
  }
  stars <- sig_stars(x)
  paste0(hetero_fmt(x), if (nzchar(stars)) paste0("$", stars, "$"))
}

panel_y2 <- hetero_panel(
  y2, "Y_2", "SDF-news PCs", w1, y1, z, z_mat, hetero_fmt, pcell
)
panel_w2 <- hetero_panel(
  w2, "W_2", "SDF-news residuals", w1, y1, z, z_mat, hetero_fmt, pcell
)

n_obs <- set_id_mean_eq$sample$n
span <- paper_sample_span(set_id_mean_eq$sample)
