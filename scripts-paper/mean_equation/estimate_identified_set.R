# Set identification of the SDF-news coefficients in the consumption-growth
# equation (Lewbel 2012 heteroskedasticity, relaxed-correlation form), per
# docs/lewbel_multivariate_set_identification.tex and the paper specification.
# Same specification as fit_ols.R -- Y1 = consumption growth, common
# conditioning X_t = (1, lagged expected-SDF PCs), Y2 = the three SDF-news PCs,
# now treated as endogenous -- with the run_pipeline.R instrument (column z_col
# of z_source()) as the single heteroskedasticity driver Z, applied to every
# news component (pair set = {news PC i} x {Z}).
#
# This file builds the estimation frame once and runs the estimator under every
# specification in PAPER_SPEC_PLAN$mean. The estimation itself lives in
# estimate_identified_set_core.R.
#
# set_id_mean_eq is the PUBLISHED specification and is what every downstream
# consumer reads; there are well over a hundred such references and none of them
# needed to change. set_id_mean_eq_by_spec carries every specification that ran,
# keyed by name, and exists so the comparison is available to diagnostics
# without any table, figure or paper number being able to reach it by accident.
# Run via run_pipeline.R after build_consumption_growth.R and build_sdf_pcs.R.
paper_source_once(paper_path(
  "mean_equation", "estimate_identified_set_core.R"
))

# aligned estimation frame; complete cases truncate the sample to the
# instrument's span
set_id_data <- list(
  gr1_pcecc96,
  lag_expected_sdf_pc,
  sdf_news_pc,
  z_source()
) |>
  purrr::reduce(dplyr::full_join, by = "qtr") |>
  filter_window() |>
  tidyr::drop_na() |>
  dplyr::arrange(qtr)
y1_col <- hetid::HETID_CONSTANTS$CONSUMPTION_GROWTH_COL
x_cols <- value_cols(lag_expected_sdf_pc)
y2_cols <- value_cols(sdf_news_pc)

set_id_mean_eq_by_spec <- stats::setNames(
  lapply(PAPER_SPEC_PLAN$mean, function(spec) {
    estimate_mean_equation(
      set_id_data, y1_col, x_cols, y2_cols, z_col,
      paper_spec_impose_null(spec)
    )
  }),
  PAPER_SPEC_PLAN$mean
)
set_id_mean_eq <- set_id_mean_eq_by_spec[[paper_published_spec("mean")]]

set_id_report_spec <- function(fit, spec, published) {
  digits <- PAPER_REPORTING_CONTROL$precision$console_significant
  cat(
    sprintf(
      "set identification [spec %s%s] (Z = %s): N =",
      spec, if (published) ", PUBLISHED" else ", diagnostic", z_col
    ),
    fit$sample$n, "over", format(fit$sample$span[1]), "to",
    format(fit$sample$span[2]),
    "\n  tau* =", signif(fit$tau_star, digits),
    if (fit$tau_star_capped) "(capped at sweep max)" else "",
    " kappa(Q) =", signif(fit$theta_point_cond, digits),
    " beta2R:", if (fit$impose_null) "null (= 0)" else "sample", "\n"
  )
  print(fit$theta_table, digits = digits)
  print(fit$relevance, digits = digits)
}

for (spec in PAPER_SPEC_PLAN$mean) {
  set_id_report_spec(
    set_id_mean_eq_by_spec[[spec]], spec,
    identical(spec, paper_published_spec("mean"))
  )
}

rm(set_id_data, y1_col, x_cols, y2_cols, spec)
